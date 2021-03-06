module Synth
    -- ( updateProg
    -- , traceToMap
    -- )
where

import BreveLang
import BreveEval

import qualified Euterpea as E

import Control.Monad (liftM, liftM2, mplus)
import qualified Data.Map.Strict as Map
import Data.Function (on)
import Data.List (find, sortBy, tails, transpose)
import Data.Maybe (catMaybes, fromMaybe, fromJust, mapMaybe)
import Data.Tuple (swap)

data Type = Faithful | Plausible deriving (Eq, Ord, Show)
data Mode = AdHoc | Live deriving (Eq, Ord, Show)

type TraceMap = Map.Map Loc Val

toTraceMap :: TraceList -> TraceMap
toTraceMap = Map.fromList . mapMaybe makePairs

makePairs :: Val -> Maybe (Loc, Val)
makePairs val = case val of
    (Vp _ (TrLoc l)) -> Just (l, val)
    (Vn _ (TrLoc l)) -> Just (l, val)
    (Vd _ (TrLoc l)) -> Just (l, val)
    _ -> Nothing

getLocs :: Trace -> [Loc]
getLocs = getLocs' []

getLocs' locs (TrLoc l) = [l]
getLocs' locs (TrUn _ t) = getLocs' locs t
getLocs' locs (TrOp _ t1 t2) = getLocs' locs t1 ++ getLocs' locs t2

-- Get all locations [[Loc]] from the user's updates
-- For each update, maybe synth for each location [[Maybe Val]]
-- Then get each of the valid substitutions for each update [[Val]]
-- If any of those are empty (at least one update had no valid synths) then
-- synthesis failed (return [] or Nothing?) (hard constraint could not be
-- satisfied)
-- Start building complete programs using the rotating heuristic. [TraceMap]
--    Say we had [[1,2,3],[1,2],[3],[2,4]]
--    [1,2,3,4], [2,1,3,4], >[3,1,3,4]<, >[1,2,3,2]<
--    Only two update sets are actually valid, though... the paper may try to
--    allow them? I think it handles multiple bindings by just doing the
--    rightmost.
-- We have to turn the [[Val]] into [[(Loc,Val]] because it's impossible to
-- compare Val's, due to how they're set up in BreveEval. Fortunately, comparing
-- locs makes a lot more sense in this domain.

-- Synthesize takes 2 arguments:
-- 1. rho, the map of locations to values in the original program
-- 2. the list of user updates
-- And returns the possible substitutions for each update.
-- That is, if we have 2 updates, the returned list will contain 2 lists of new
-- values that may be used. Deciding which one to actually use is based on the
-- manipulation mode (ad hoc or live) and the synthesis type (faithful or
-- plausible) which are determined by whatever calls this function.

synthesize :: Type -> Mode -> TraceList -> TraceList -> Maybe [TraceMap]
synthesize t m traces updates = let
    rho = toTraceMap traces
    allLocs = map (getLocs . getTrace) updates
    synthRes = map catMaybes $ zipWith (map . solveSimple rho) updates allLocs
    doType = case t of
        Faithful -> if any null synthRes then Nothing else Just synthRes
        Plausible -> if all null synthRes then Nothing else Just synthRes
    in
    case (doType, m) of
    (Nothing, _) -> Nothing
    (Just s, AdHoc) -> Just $ adhocMode traces s
    (Just s, Live) -> Just $ liveMode s

-- Live mode uses the rotating heuristic to return exactly one substitution.
liveMode :: [[Val]] -> [TraceMap]
liveMode updates = [Map.fromList $ rotate1 (map (mapMaybe makePairs) updates)]

-- Ad hoc mode uses a ranking heuristic to sort each update, then leaves
-- actually choosing which update to use to the caller. (i.e. the user)
-- Since we're using a tracemap, it's a bit pointless to rank. It would be a
-- better idea for the caller to handle the ranking instead.
adhocMode :: TraceList -> [[Val]] -> [TraceMap]
adhocMode rho = map (Map.fromList . mapMaybe makePairs . rankUpdates rho)

-- rankUpdates takes a list of all traces in the program and a substitution
-- generated from synthesis (containing all the possible substitutions that
-- would satisfy the particular update) and returns a TraceList, built from the
-- TraceMap, sorted by the "footprint" of each substitution pair. That is, the
-- elements are sorted by how many other values are affected by that element.
-- NOTE: The initial trace list should be ALL of the traces in the program, not
-- just the numeric traces!
rankUpdates :: TraceList -> TraceList -> TraceList
rankUpdates allTraces substitution = let
    locCount :: Map.Map Loc Int
    allLocs = concatMap (getLocs . getTrace) allTraces
    locCount = foldl (\m loc -> Map.insertWith (+) loc 1 m) Map.empty allLocs
    rank (Vp _ (TrLoc l)) = fromJust $ Map.lookup l locCount
    rank (Vn _ (TrLoc l)) = fromJust $ Map.lookup l locCount
    rank (Vd _ (TrLoc l)) = fromJust $ Map.lookup l locCount in
    sortBy (\a b -> if rank a < rank b then LT else GT) substitution

-- faithful + live mode is generally the most common synthesis paradigm, based
-- on what I see from sketch-n-sketch
synthFaithfulLive :: TraceList -> TraceList -> Maybe TraceMap
synthFaithfulLive rho updates = fmap head $ synthesize Faithful Live rho updates

rotate1 :: Eq a => [[a]] -> [a]
rotate1 = foldl (\ls as -> ls ++ [fromMaybe (head as) (as `firstNotIn` ls)]) []

rotate :: Eq a => [[a]] -> [[a]]
rotate lls = let
    (longest : rest) = sortBy (flip compare `on` length) lls in -- sorts w/ longest first
    map (\l -> rotate1 (l : rest)) (init $ tails longest)

firstNotIn :: Eq a => [a] -> [a] -> Maybe a
firstNotIn list filt = find (not . (`elem` filt)) list

-- solveSimple takes a substitution map (locations -> values in the original
-- program), a value to solve for and a location for that value. It uses inverse
-- operations to solve for a new value for the given location.
-- It returns Just Val if a new value is found for that location, and Nothing
-- otherwise.
solveSimple :: TraceMap -> Val -> Loc -> Maybe Val
solveSimple rho val loc = case getTrace val of
    (TrLoc l') -> if loc == l' then Just val else Nothing
    (TrUn op t) -> solveSimple rho (invUnOp op val t) loc
    (TrOp op t1 t2) -> let (i, j) = (evalTrace rho t1, evalTrace rho t2) in
        (flip (solveSimple rho) loc =<< solveForJ op val i t2) `mplus`
        (flip (solveSimple rho) loc =<< solveForI op val j t1)

invUnOp :: UnOp -> Val -> Trace -> Val
invUnOp Neg (Vn n _) = Vn (-n)
invUnOp Neg (Vd d _) = Vd (-d)

-- n = i op j. For commutative ops, it doesn't matter how you invert it, really.
-- But consider this case:
-- 6 = 8 - 2 (val = t1 `Sub` t2)
-- 8 - 6 = 2 (t1 `Sub` val = t2) (solve for j)
-- 6 + 2 = 8 (val `Add` t2 = t1) (solve for i)
-- More formally, subtraction is anti-commutative. (Division isn't.)

solveForJ :: BinOp -> Val -> Maybe Val -> Trace -> Maybe Val
solveForJ _ _ Nothing = const Nothing
-- Tranposition (Add Vp Vn) is weird. The syntax is ONLY Vp + Vn. So the types
-- aren't parellel like for the other ones.
-- solveForI is the "inverse" of evalBinOp Add Vp Vn, while solveForJ is the
-- more interesting... something. Not inverse.
solveForJ Add (Vp p _) (Just (Vp i _)) = Just . Vn (toInteger $ E.pcToInt p - E.pcToInt i)

solveForJ Add (Vn n _) (Just (Vn i _)) = Just . Vn (n - i)
solveForJ Add (Vd n _) (Just (Vn i _)) = Just . Vd (n - fromInteger i)
solveForJ Add (Vn n _) (Just (Vd i _)) = Just . Vd (fromInteger n - i)
solveForJ Add (Vd n _) (Just (Vd i _)) = Just . Vd (n - i)

solveForJ Sub (Vn n _) (Just (Vn i _)) = Just . Vn (i - n)
solveForJ Sub (Vd n _) (Just (Vn i _)) = Just . Vd (fromInteger i - n)
solveForJ Sub (Vn n _) (Just (Vd i _)) = Just . Vd (i - fromInteger n)
solveForJ Sub (Vd n _) (Just (Vd i _)) = Just . Vd (i - n)

solveForJ Mult (Vn n _) (Just (Vn i _)) = Just . Vn (n `div` i)
solveForJ Mult (Vd n _) (Just (Vn i _)) = Just . Vd (n / fromInteger i)
solveForJ Mult (Vn n _) (Just (Vd i _)) = Just . Vd (fromInteger n / i)
solveForJ Mult (Vd n _) (Just (Vd i _)) = Just . Vd (n / i)

solveForJ Div (Vn n _) (Just (Vn i _)) = Just . Vn (i `div` n)
solveForJ Div (Vd n _) (Just (Vn i _)) = Just . Vd (fromInteger i / n)
solveForJ Div (Vn n _) (Just (Vd i _)) = Just . Vd (i / fromInteger n)
solveForJ Div (Vd n _) (Just (Vd i _)) = Just . Vd (i / n)

solveForI :: BinOp -> Val -> Maybe Val -> Trace -> Maybe Val
solveForI _ _ Nothing = const Nothing

solveForI Add (Vp p _) (Just (Vn n _)) = Just . Vp (pitches !! ((E.pcToInt p - fromInteger n) `mod` 12))
    where pitches = [E.C, E.Cs, E.D, E.Ds, E.E, E.F, E.Fs, E.G, E.Gs, E.A, E.As, E.B]
solveForI Add n j = solveForJ Add n j

solveForI Mult n j = solveForJ Mult n j

solveForI Sub (Vn n _) (Just (Vn j _)) = Just . Vn (j + n)
solveForI Sub (Vd n _) (Just (Vn j _)) = Just . Vd (fromInteger j + n)
solveForI Sub (Vn n _) (Just (Vd j _)) = Just . Vd (j + fromInteger n)
solveForI Sub (Vd n _) (Just (Vd j _)) = Just . Vd (j + n)

solveForI Div (Vn n _) (Just (Vn j _)) = Just . Vn (j * n)
solveForI Div (Vd n _) (Just (Vn j _)) = Just . Vd (fromInteger j * n)
solveForI Div (Vn n _) (Just (Vd j _)) = Just . Vd (j * fromInteger n)
solveForI Div (Vd n _) (Just (Vd j _)) = Just . Vd (j * n)


evalTrace :: TraceMap -> Trace -> Maybe Val
evalTrace rho (TrLoc l) = Map.lookup l rho
evalTrace rho (TrUn op t) = liftM (evalUnOp op) (evalTrace rho t)
evalTrace rho (TrOp op t1 t2) = liftM2 (evalBinOp op) (evalTrace rho t1) (evalTrace rho t2)

swapTrace :: Trace -> Val -> Val
swapTrace t v = case v of
    (Vp p _) -> Vp p t
    (Vn n _) -> Vn n t
    (Vd d _) -> Vd d t

getFirstLoc :: Trace -> Loc
getFirstLoc (TrLoc l) = l
getFirstLoc (TrUn _ t) = getFirstLoc t
getFirstLoc (TrOp _ t1 t2) = getFirstLoc t2

-- test :: Maybe Statement
-- test =
--     let source = "main = 3.5 + (1.0 + 0.5)"
--         prog = parse source
--         (res,t) = parseEval source
--         update = [Vd 6.5 (getTrace $ head t)]
--         changes = synthFaithful (toTraceMap t) update in
--     case changes of
--     [] -> Nothing
--     (c:cs) -> Just $ updateProgram c prog

test2 =
    let source = "dfsa = arpeggio([0,4,7], (D 4 1/2)); main = line(dfsa) :+: (rest 1/4) :+: chord(dfsa);"
        prog = parse source
        (res, t) = parseEval source
        update = [Vp (read "F") (TrOp Add (TrLoc (1,29)) (TrLoc (1,21)))
                 ,Vn 4 (TrLoc (1,34))]
        synthed = fromJust $ synthFaithfulLive t update in
    updateProgram synthed prog

updateProgram :: TraceMap -> Statement -> Statement
updateProgram substm (Seq ss) = Seq (map (updateProgram substm) ss)
updateProgram substm (Assign n e) = Assign n (updateExpr substm e)
updateProgram substm (Return e) = Return (updateExpr substm e)

updateExpr :: TraceMap -> Expr -> Expr
updateExpr m e = let updateE = updateExpr m in
    case e of
    (PitchClass _ l) -> maybe e (\(Vp p _) -> PitchClass p l) (Map.lookup l m)
    (N _ l) -> maybe e (\(Vn n _) -> N n l) (Map.lookup l m)
    (D _ l) -> maybe e (\(Vd d _) -> D d l) (Map.lookup l m)
    (B _) -> e
    (Note p o d) -> Note (updateE p) (updateE o) (updateE d)
    (Rest d) -> Rest (updateE d)
    (UnOpExpr op a) -> UnOpExpr op (updateE a)
    (BinOpExpr op a b) -> BinOpExpr op (updateE a) (updateE b)
    (Snippet ss) -> Snippet $ map updateE ss
    (List ls) -> List $ map updateE ls
    (Var _) -> e
    (Lambda {}) -> e
    (App s args) -> App s (map updateE args)
    (If c t f) -> If (updateE c) (updateE t) (updateE f)
    (Case c ps) -> Case (updateE c) (map (\(p,e) -> (p, updateE e)) ps)
