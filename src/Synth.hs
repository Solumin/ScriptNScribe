module Synth
    -- ( updateProg
    -- , traceToMap
    -- )
where

import BreveLang
import BreveEval

import qualified Euterpea as E

import Control.Monad (ap, liftM, liftM2, mplus)
import qualified Data.Map.Strict as Map
import Data.Maybe (fromMaybe, mapMaybe)
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
synthFaithful rho ups = liftM (\v -> Map.insert loc v rho) val
    where
        up = head ups
        tr = getTrace up
        loc = getFirstLoc tr
        val = solveSimple rho loc up -- fail synth instead of orig val

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

test :: Maybe TraceMap
test =
    let (p,t) = parseEval "main = 3.5 + (1.0 + 0.5)"
        update = [Vd 6.5 (getTrace $ head t)] in
    synthFaithful (toTraceMap t) update

{-
 - This is going to be really simple for now. Very, very basic tracing.
 - Really all we're going to do is take the changes and do a quick substitution
 - to make the new AST.
 - It's very low-powered and nowhere near the final project.
 -
 - Given:
 - Program (AST)
 - Traces
 - Updates

traceToMap :: Traces -> Map.Map Loc Expr
traceToMap ts = Map.fromList $ map (\s -> case s of
    pc@(PitchClass p l) -> (l, pc)
    ot@(Octave o l) -> (l, ot)
    dr@(Duration d l) -> (l, dr)) ts

updateProg :: Statement -> Traces -> Traces -> Statement
updateProg prog oldTraces newTraces = subst prog (traceToMap newTraces)

subst :: Statement -> Map.Map Loc Expr -> Statement
subst (Seq ss) m = Seq (map (\s -> subst s m) ss)
subst (v := s) m = (v := substExpr s m)

substExpr :: Expr -> Map.Map Loc Expr -> Expr
substExpr e@(PitchClass _ l) m = maybe e (id) (Map.lookup l m)
substExpr e@(Octave _ l) m = maybe e (id) (Map.lookup l m)
substExpr e@(Duration _ l) m = maybe e (id) (Map.lookup l m)
substExpr (Note p o d) m = Note (substExpr p m) (substExpr o m) (substExpr d m)
substExpr (Rest d) m = Rest (substExpr d m)
substExpr (Snippet es) m = Snippet (map (\e -> substExpr e m) es)
substExpr e _ = e
-}
