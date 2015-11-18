module Synth
    ( updateProg
    , traceToMap
    )
where

import BreveLang
import qualified Data.Map.Strict as Map
import Data.Tuple (swap)

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
 -}

import BreveLang

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
