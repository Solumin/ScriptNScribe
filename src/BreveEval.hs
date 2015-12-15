module BreveEval
    -- ( run
    -- , runEnv
    -- , interp
    -- ) where
    where
import BreveLang
import BrevePrelude
import qualified Euterpea (play, line)
import qualified Euterpea.Music.Note.Music as E
import Text.Parsec (runParser)

import Control.Monad (msum)
import Data.Function (on)
import Data.List (intercalate, nubBy)
import qualified Data.Map as Map
import Data.Maybe (fromMaybe)

type Music = E.Music E.Pitch

type Binding = (String, Val)
type EvalRes = [Binding]
type TraceList = [Val]
type Env = (EvalRes, TraceList)

emptyEnv :: Env
emptyEnv = ([],[])

data Trace = TrLoc Loc | TrOp BinOp Trace Trace | TrUn UnOp Trace deriving (Eq, Read, Show)

-- instance Show Trace where
--     show (TrLoc l) = show l
--     show (TrOp op a b) = '(' : shows a (' ' : shows op (' ' : shows b ")"))
--     show (TrUn Neg x) = "Neg" ++ (show x)
--     show (TrUn Not x) = "Not" ++ (show x)

data Val = Vp E.PitchClass Trace
         | Vn Integer Trace
         | Vd Double Trace
         | Vb Bool
         | Vnote Val Val Val-- Note
         | Vrest Val        -- Rest
         | Vseq Val Val     -- represents :+: (and therefore Snippet)
         | Vpar Val Val     -- represents the :=: operator for rest, notes and snippets
         | Vlist [Val]      -- Expr List
         | Vfunc Expr       -- Lambda

instance Show Val where
    show (Vp p t) = "Pitch " ++ shows p (' ' : show t)
    show (Vn n t) = "Int " ++ shows n (' ' : show t)
    show (Vd d t) = "Double " ++ shows d (' ' :  show t)
    show (Vb b) = "Val_Bool <" ++ shows b ">"
    show (Vnote p o d) = "Val_Note <" ++ unwords (map show [p,o,d]) ++ ">"
    show (Vrest r) = "Val_Rest <" ++ shows r ">"
    show (Vseq a b) = '(' : shows a " :+: " ++ shows b ")"
    show (Vpar a b) = '(' : shows a " :=: " ++ shows b ")"
    show (Vlist l) = "Vlist [" ++ intercalate ", " (map show l) ++ "]"
    show (Vfunc l) = "Vfunc <" ++ shows l ">"

instance Eq Val where
    (Vp a _) == (Vp b _) = a == b
    (Vn a _) == (Vn b _) = a == b
    (Vn a _) == (Vd b _) = fromInteger a == b
    a@(Vd _ _) == b@(Vn _ _) = b == a
    (Vd a _) == (Vd b _) = a == b
    (Vb a) == (Vb b) = a == b
    (Vnote a b c) == (Vnote d e f) = and $ zipWith (==) [a,b,c] [d,e,f]
    (Vrest a) == (Vrest b) = a == b
    (Vseq a b) == (Vseq c d) = a == c && b == d
    (Vpar a b) == (Vpar c d) = a == c && b == d
    (Vlist a) == (Vlist b) = a == b
    a == b = error $ unwords ["Cannot compare", show a, "and", show b]

instance Ord Val where
    (Vp a _) <= (Vp b _) = a <= b
    (Vn a _) <= (Vn b _) = a <= b
    (Vn a _) <= (Vd b _) = fromInteger a <= b
    (Vd a _) <= (Vn b _) = a <= fromInteger b
    (Vd a _) <= (Vd b _) = a <= b
    (Vb a) <= (Vb b) = a <= b
    (Vnote a b c) <= (Vnote d e f) = or (zipWith (<=) [a,b,c] [d,e,f])
    (Vrest a) <= (Vrest b) = a <= b
    (Vpar a b) <= (Vpar c d) = a <= c && b <= d
    (Vseq a b) <= (Vseq c d) = a <= c && b <= d
    (Vlist a) <= (Vlist b) = a <= b
    a <= b = error $ unwords ["Cannot compare", show a, "and", show b]

getTrace :: Val -> Trace
getTrace val = case val of
    (Vp _ t) -> t
    (Vn _ t) -> t
    (Vd _ t) -> t

-- Takes source code and parses it to generate the AST and parse traces
parse :: String -> Statement
parse input = case runParser breveParser () "input" input of
    Left err -> error (show err)
    Right st -> st

-- Produce the Environment defined by a program.
parseEval :: String -> Env
parseEval = parseEvalEnv initEnv

parseEvalEnv :: Env -> String -> Env
parseEvalEnv env source =
    let prog = parse source
        (evalRes, exprTraces) = eval env prog in
    (evalRes, nubBy ((==) `on` getTrace) exprTraces)

-- Prelude contains many useful functions, like map and fold, written purely in
-- Breve. initEnv loads the prelude so it can be used in parseEval.
initEnv = parseEvalEnv emptyEnv prelude

-- Takes source code and evaluates the "main" expression (load Prelude)
run :: String -> Val
run = runEnv initEnv

-- Same as run, but with a given initial environment. (i.e. no Prelude)
runEnv :: Env -> String -> Val
runEnv env source =
    fromMaybe (error "No main to evaluate!") (lookup "main" (fst $ parseEvalEnv env source))

-- Transforms a Val into a Music object that can be played.
toMusic :: Val -> Music
toMusic (Vnote (Vp p _) o d) = E.note (valToDur d) (p, valToOct o)
toMusic (Vrest d) = E.rest (valToDur d)
toMusic (Vseq a b) = toMusic a E.:+: toMusic b
toMusic (Vpar a b) = toMusic a E.:=: toMusic b
toMusic v = error $ "Cannot create a music object from " ++ show v

-- Performs the Music represented by the program.
perform :: String -> IO()
perform = Euterpea.play . toMusic . run

-- ==========
-- Evaluating
-- ==========

-- Given an environment, evaluate the given statement.
-- Assign statements create a binding between the name and the evaluated
-- expression.
-- Return statements do the same with the name "return" -- which means there
-- must only be one per function, at the end of the function body!
-- Sequence statements are evaluated top-to-bottom, building the environment as
-- each component statement is evaluated.
eval :: Env -> Statement -> Env
eval env (Seq ss) = foldl eval env ss
eval env@(bs, ts) (Assign n e) = let (val, traces) = evalExpr env e in ((n,val):bs, traces ++ ts)
eval env@(bs, ts) (Return e) = let (val, traces) = evalExpr env e in (("return",val):bs, traces ++ ts)

-- logIt is a predicate for determining if a Val should be added to the traces.
-- Only those Vals that have Trace in their definition are logged.
logIt :: Val -> Bool
logIt val = case val of
    (Vp _ _) -> True
    (Vn _ _) -> True
    (Vd _ _) -> True
    (Vb _) -> False
    (Vnote p o d) -> False
    (Vrest d) -> False
    (Vseq v1 v2) -> False
    (Vpar v1 v2) -> False
    (Vlist vs) -> False
    (Vfunc _) -> False

evalExpr :: Env -> Expr -> (Val, TraceList)
evalExpr env expr =
    let evalE = evalExpr env
        both v = (v, [v])   -- v is returned and added to trace
        with (v,t) tr = (v, t ++ tr) -- combine traces
        only v = (v, []) in -- return only v
    case expr of
    (PitchClass p l) -> let v = Vp p (TrLoc l) in both v
    (N n l) -> let v = Vn n (TrLoc l) in both v
    (D d l) -> let v = Vd d (TrLoc l) in both v
    (B b) -> only $ Vb b
    (UnOpExpr op e) ->
        let (v1, t) = evalE e
            res = evalUnOp op v1 in
        if logIt res then with (both res) t else with (only res) t
    (BinOpExpr op e1 e2) ->
        let (v1, t1) = evalE e1
            (v2, t2) = evalE e2
            res = evalBinOp op v1 v2 in
        if logIt res then with (both res) (t1 ++ t2) else with (only res) (t1 ++ t2)
    (Note p o d) ->
        let (p', tp) = evalE p
            (o', to) = evalE o
            (d', td) = evalE d
            n = note p' o' d' in
        (n, tp ++ to ++ td)
    (Rest d) -> let (d', td) = evalE d in (rest d', td)
    (Snippet ss) ->
        let (vals, traces) = unzip $ map evalE ss in (snippet vals, concat traces)
    (List ls) ->
        let (vals, traces) = unzip $ map evalE ls in (Vlist vals, concat traces)
    (Var s) -> let v = lookupVar env s in if logIt v then both v else only v
    l@(Lambda _ _) -> only $ Vfunc l
    (App name args) ->
        let (vargs, traces) = unzip $ map evalE args
            res = evalApp env name vargs in
        -- if is loggable, res's tracelist contains res already
        with res (concat traces)
    (If c t f) ->
        let (cond, tc) = evalE c
            res = evalE $ evalIf cond t f in
        with res tc
    (Case c cases) ->
        let (cond, tc) = evalE c
            res = evalCase env cond cases in
        with res tc

note :: Val -> Val -> Val -> Val
note p@(Vp{}) o@(Vn{}) d = case d of
    (Vd{}) -> Vnote p o d
    (Vn{}) -> Vnote p o d
    _ -> error $ "Expected a numeric duration, received " ++ show d
note _ _ _ = error "Note takes a pitch class, an integer octave and a numeric duration"

rest :: Val -> Val
rest v@(Vd d _) = Vrest v
rest v@(Vn n _) = Vrest v
rest _ = error "Rests take a numeric duration"

valToDur :: Val -> E.Dur
valToDur (Vd d _) = toRational d
valToDur (Vn n _) = toRational n
valToDur _ = error "Durations must be numeric types"

valToOct :: Val -> E.Octave
valToOct (Vn n _) = fromInteger n
valToOct _ = error "Octaves must be integers"

snippet :: [Val] -> Val
snippet (v@(Vnote{}):vs) = case vs of
    [] -> v
    _ -> Vseq v (snippet vs)
snippet (v@(Vrest _):vs) = case vs of
    [] -> v
    _ -> Vseq v (snippet vs)
snippet (_:vs) = error "A snippet should only contain Note or Rest objects"

lookupVar :: Env -> String -> Val
lookupVar env name = fromMaybe (error $ shows name " is undefined." ++ show env) (lookup name $ fst env)

evalApp :: Env -> String -> [Val] -> (Val, TraceList)
evalApp env@(bs,ts) name args =
    let (Vfunc (Lambda params body)) = lookupVar env name
        nonexhaust = "Non-exhaustive patterns in function " ++ name
        argenv = concat $ fromMaybe (error nonexhaust) (traverse matchCase (zip params args))
        (evalRes, traces) = eval (argenv ++ bs, ts) body
        res = fromMaybe
                (error $ "Function " ++ name ++ " has no return statement!")
                (lookup "return" evalRes)
        in
    (res, traces)

evalIf :: Val -> Expr -> Expr -> Expr
evalIf c t f = case c of
    (Vb True) -> t
    (Vb False) -> f
    _ -> error "Breve is not 'truthy'; conditions must evaluate to bool."

evalCase :: Env -> Val -> [(Pat, Expr)] -> (Val, TraceList)
evalCase env@(bs,ts) cond cases =
    -- hlint iterated the following line a good 3-4 times before coming up with
    -- this. Started with "map matchCase $ zip (map fst cases) (repeat c)"
    let envs = map ((\ c -> curry matchCase c cond) . fst) cases
        -- Take [Maybe Env] and [(Pat, Expr)], produce [Maybe (Expr, Env)]
        evals = zipWith (\ m p -> fmap ((,) (snd p)) m) envs cases in
    case msum evals of
        Just (expr, env') -> evalExpr (env' ++ bs, ts) expr
        Nothing -> error "Non-exhaustive patterns in Breve case statement."

matchCase :: (Pat, Val) -> Maybe EvalRes
matchCase (p,v) =
    let joinEnv a b = (++) <$> a <*> b in
    case (p,v) of
    (Ppc p, Vp p' _) -> if p == p' then Just [] else Nothing
    (Pn n, Vn n' _) -> if n == n' then Just [] else Nothing
    (Pd d, Vd d' _) -> if d == d' then Just [] else Nothing
    (Pb b, Vb b') -> if b == b' then Just [] else Nothing
    (Pnote p o d, Vnote p' o' d') ->
       concat <$> mapM matchCase [(p, p'), (o, o'), (d, d')]
    (Prest d, Vrest d') -> matchCase (d, d')
    (Psplit a r, Vlist (v:vs)) -> joinEnv (matchCase (a, v)) (matchCase (r, Vlist vs))
    (Psplit a r, Vseq h t) -> joinEnv (matchCase (a, h)) (matchCase (r, t))
    (Psplit a (Psnip []), v) -> matchCase (a, v)
    (Plist (l:ls), Vlist (v:vs)) ->
        if length ls == length vs
        then joinEnv (matchCase (l,v)) (matchCase (Plist ls, Vlist vs))
        else Nothing
    (Plist [], Vlist []) -> Just []
    (Psnip (s:ss), Vseq h t) -> joinEnv (matchCase (s,h)) (matchCase (Psnip ss, t))
    (Psnip [s], Vnote{}) -> matchCase(s, v)
    (Psnip [s], Vrest _) -> matchCase(s, v)
    -- (Psnip [], _) -> There's nothing to match an empty Psnip to! Instead,
    -- match single element split pattern
    (Pvar s, v) -> Just [(s, v)]
    (Ppat s p, v) -> joinEnv (Just [(s, v)]) (matchCase (p,v))
    (Pwc, _) -> Just []
    _ -> Nothing

evalUnOp :: UnOp -> Val -> Val
evalUnOp Not v = case v of
    (Vb b) -> Vb (not b)
    _ -> error "\"Not\" takes boolean"
evalUnOp Neg v = case v of
    (Vn n l) -> Vn (-n) (TrUn Neg l)
    (Vd d l) -> Vd (-d) (TrUn Neg l)
    _ -> error "Negation is defined for only numbers"

evalBinOp :: BinOp -> Val -> Val -> Val

evalBinOp Add (Vd d1 l1) (Vd d2 l2) = Vd (d1 + d2) (TrOp Add l1 l2)
evalBinOp Add (Vd d1 l1) (Vn n2 l2) = Vd (d1 + fromInteger n2) (TrOp Add l1 l2)
evalBinOp Add (Vn n1 l1) (Vd d2 l2) = Vd (fromInteger n1 + d2) (TrOp Add l1 l2)
evalBinOp Add (Vn n1 l1) (Vn n2 l2) = Vn (n1 + n2) (TrOp Add l1 l2)
-- Using the behavior of Euterpea's pitch and trans functions, which always
-- return a sharp note instead of a flat enharmonic.
evalBinOp Add (Vp pc l1) (Vn n l2) = Vp (pitches !! ((E.pcToInt pc + d) `mod` 12)) (TrOp Add l1 l2)
    where
        pitches = [E.C, E.Cs, E.D, E.Ds, E.E, E.F, E.Fs, E.G, E.Gs, E.A, E.As, E.B]
        d = fromInteger n

evalBinOp Sub (Vd d1 l1) (Vd d2 l2) = Vd (d1 - d2) (TrOp Sub l1 l2)
evalBinOp Sub (Vd d1 l1) (Vn n2 l2) = Vd (d1 - fromInteger n2) (TrOp Sub l1 l2)
evalBinOp Sub (Vn n1 l1) (Vd d2 l2) = Vd (fromInteger n1 - d2) (TrOp Sub l1 l2)
evalBinOp Sub (Vn n1 l1) (Vn n2 l2) = Vn (n1 - n2) (TrOp Sub l1 l2)
evalBinOp Sub vp@(Vp pc l1) vn@(Vn n l2) = evalBinOp Add vp (evalUnOp Neg vn)

evalBinOp Mult (Vd d1 l1) (Vd d2 l2) = Vd (d1 * d2) (TrOp Mult l1 l2)
evalBinOp Mult (Vd d1 l1) (Vn n2 l2) = Vd (d1 * fromInteger n2) (TrOp Mult l1 l2)
evalBinOp Mult (Vn n1 l1) (Vd d2 l2) = Vd (fromInteger n1 * d2) (TrOp Mult l1 l2)
evalBinOp Mult (Vn n1 l1) (Vn n2 l2) = Vn (n1 * n2) (TrOp Mult l1 l2)

-- Note that Div always returns a double!!
evalBinOp Div (Vd d1 l1) (Vd d2 l2) = Vd (d1 / d2) (TrOp Div l1 l2)
evalBinOp Div (Vd d1 l1) (Vn n2 l2) = Vd (d1 / fromInteger n2) (TrOp Div l1 l2)
evalBinOp Div (Vn n1 l1) (Vd d2 l2) = Vd (fromInteger n1 / d2) (TrOp Div l1 l2)
evalBinOp Div (Vn n1 l1) (Vn n2 l2) = Vd (fromInteger n1 / fromInteger n2) (TrOp Div l1 l2)

evalBinOp SeqOp a b = Vseq (check a) (check b)
    where
    check v = case v of
        (Vnote{}) -> v
        (Vrest _) -> v
        (Vseq _ _) -> v
        (Vpar _ _) -> v
        _ -> error $ ":+: is undefined for argument " ++ show v

evalBinOp ParOp a b = Vpar (check a) (check b)
    where
    check v = case v of
        (Vnote{}) -> v
        (Vrest _) -> v
        (Vseq _ _) -> v
        (Vpar _ _) -> v
        _ -> error $ ":=: is undefined for argument " ++ show v

evalBinOp Eq  a b = Vb (a == b)
evalBinOp Neq a b = Vb (a /= b)
evalBinOp Lt  a b = Vb (a <  b)
evalBinOp Lte a b = Vb (a <= b)
evalBinOp Gt  a b = Vb (a >  b)
evalBinOp Gte a b = Vb (a >= b)

evalBinOp Cons a (Vlist l) = Vlist (a : l)

evalBinOp Cat (Vlist a) (Vlist b) = Vlist (a ++ b)

evalBinOp o a b = error $ unwords ["Op", show o, "is undefined for args", show a, show b]
