module Synth
    -- ( updateProg
    -- , traceToMap
    -- )
where

import BreveLang
import BreveEval

import qualified Euterpea as E

import Control.Monad (liftM, liftM2, mplus, zipWithM)
import qualified Data.Map.Strict as Map
import Data.List ((\\))
import Data.Maybe (catMaybes, fromMaybe, mapMaybe)
import Data.Tuple (swap)

type TraceMap = Map.Map Loc Val

toTraceMap :: TraceList -> TraceMap
toTraceMap = Map.fromList . mapMaybe makePairs
    where
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

synthFaithful :: TraceMap -> TraceList -> Maybe TraceMap
synthFaithful rho updates = let
    allLocs = map (getLocs . getTrace) updates
    chosen = chooseSemiUnique allLocs
    newTraces = zipWithM (solveSimple rho) chosen updates in
    case newTraces of
        Nothing -> Nothing
        Just vals -> Just $ foldl (\m (l,v) -> Map.insert l v m) rho (zip chosen vals)

-- synthFaithful :: TraceMap -> TraceList -> Maybe TraceMap
-- synthFaithful rho ups = liftM (\v -> Map.insert loc v rho) val
--     where
--         up = head ups
--         tr = getTrace up
--         loc = getFirstLoc tr
--         val = solveSimple rho loc up -- fail synth instead of orig val

-- Choose the first element of the sublist that has not been selected previously
-- from another list.
-- If all elements from the sublist have already been used, choose the first
-- one.
chooseSemiUnique :: Eq a => [[a]] -> [a]
chooseSemiUnique =
    foldl (\cs ls -> cs ++
            [if all (`elem` cs) ls then head ls else head $ filter (not . (`elem` cs)) ls])
        []

solveSimple :: TraceMap -> Loc -> Val -> Maybe Val
solveSimple rho loc val = case getTrace val of
    (TrLoc l') -> if loc == l' then Just val else Nothing
    (TrUn op t) -> solveSimple rho loc (invUnOp op val t)
    (TrOp op t1 t2) -> let (i, j) = (evalTrace rho t1, evalTrace rho t2) in
        (solveSimple rho loc =<< solveForJ op val i t2) `mplus`
        (solveSimple rho loc =<< solveForI op val j t1)

invUnOp :: UnOp -> Val -> Trace -> Val
invUnOp Neg (Vn n _) = Vn (-n)
invUnOp Neg (Vd d _) = Vd (-d)

-- n = i op j. For commutative ops, it doesn't matter how you invert it, really.
-- But consider this case:
-- 6 = 8 - 2 (val = t1 `Sub` t2)
-- 8 - 6 = 2 (t1 `Sub` val = t2) (solve for j)
-- 6 + 2 = 8 (val `Add` t2 = t1) (solve for i)

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

test :: Maybe Statement
test =
    let source = "main = 3.5 + (1.0 + 0.5)"
        prog = fst $ parse source
        (res,t) = parseEval source
        update = [Vd 6.5 (getTrace $ head t)]
        changes = synthFaithful (toTraceMap t) update in
    case changes of
    Nothing -> Nothing
    Just c -> Just $ updateProgram c prog

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
