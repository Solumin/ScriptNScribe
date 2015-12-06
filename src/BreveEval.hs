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

import Data.Maybe (fromMaybe, catMaybes)
import Control.Monad (zipWithM, mplus, msum)
import Data.List (intercalate)
import Data.Monoid (mappend)

type Music = E.Music E.Pitch

type Binding = (String, Val)
type Env = [Binding]

dummyLoc = TrLoc (-1,-1)

data Trace = TrLoc Loc | TrOp BinOp Trace Trace | TrUn UnOp Trace deriving (Show, Eq)

data Val = Vp E.PitchClass Trace
         | Vn Integer Trace
         | Vd Double Trace
         | Vb Bool
         | Vnote Music     -- Note
         | Vrest Music     -- Rest
         | Vseq Val Val   -- represents :+: (and therefore Snippet)
         | Vpar Val Val   -- represents the :=: operator for rest, notes and snippets
         | Vlist [Val]     -- Expr List
         | Vfunc Expr      -- Lambda

instance Show Val where
    show (Vp p t) = "Val_PitchClass <" ++ shows p (shows t ">")
    show (Vn n t) = "Val_Integer <" ++ shows n (shows t ">")
    show (Vd d t) = "Val_Double <" ++ shows d (shows t ">")
    show (Vb b) = "Val_Bool <" ++ shows b ">"
    show (Vnote e) = "Val_Note <" ++ shows e ">"
    show (Vrest r) = "Val_Rest <" ++ shows r ">"
    show (Vseq a b) = '(' : shows a " :+: " ++ shows b ")"
    show (Vpar a b) = '(' : shows a " :=: " ++ shows b ")"
    show (Vlist l) = "Vlist [" ++ intercalate ", " (map show l) ++ "]"
    show (Vfunc l) = show l

instance Eq Val where
    (Vp a _) == (Vp b _) = a == b
    (Vn a _) == (Vn b _) = a == b
    (Vn a _) == (Vd b _) = fromInteger a == b
    a@(Vd _ _) == b@(Vn _ _) = b == a
    (Vd a _) == (Vd b _) = a == b
    (Vb a) == (Vb b) = a == b
    (Vnote a) == (Vnote b) = a == b
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
    (Vnote a) <= (Vnote b) = a <= b
    (Vrest a) <= (Vrest b) = a <= b
    (Vpar a b) <= (Vpar c d) = a <= c && b <= d
    (Vseq a b) <= (Vseq c d) = a <= c && b <= d
    (Vlist a) <= (Vlist b) = a <= b
    a <= b = error $ unwords ["Cannot compare", show a, "and", show b]

-- Takes source code and parses it to generate the AST and parse traces
parse :: String -> (Statement, Traces)
parse input = case runParser breveParser [] "input" input of
    Left err -> error (show err)
    Right st -> st

-- Produce the Environment defined by a program.
-- TODO Load the Prelude environment
parseEval :: String -> Env
parseEval = parseEvalEnv initEnv

parseEvalEnv :: Env -> String -> Env
parseEvalEnv env = eval env . fst . parse

initEnv = parseEvalEnv [] prelude

-- Takes source code and evaluates the "main" expression.
run :: String -> Val
run = runEnv initEnv

-- Same as run, but with a given initial environment.
runEnv :: Env -> String -> Val
runEnv env source =
    fromMaybe (error "No main to evaluate!") (lookup "main" (parseEvalEnv env source))

-- Transforms a Val into a Music object that can be played.
toMusic :: Val -> Music
toMusic (Vnote n) = n
toMusic (Vrest r) = r
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
eval env (Assign n e) = (n, evalExpr env e) : env
eval env (Return e) = ("return", evalExpr env e) : env

evalExpr :: Env -> Expr -> Val
evalExpr env expr = let evalE = evalExpr env in
    case expr of
    (PitchClass p l) -> Vp p (TrLoc l)
    (N n l) -> Vn n (TrLoc l)
    (D d l) -> Vd d (TrLoc l)
    (B b) -> Vb b
    (UnOpExpr op e) -> evalUnOp op (evalExpr env e)
    (BinOpExpr op e1 e2) -> evalBinOp op (evalExpr env e1) (evalExpr env e2)
    (Note p o d) -> note (evalE p) (evalE o) (evalE d)
    (Rest d) -> rest (evalE d)
    (Snippet ss) -> snippet (map evalE ss)
    (List ls) -> Vlist (map evalE ls)
    (Var s) -> lookupVar env s
    l@(Lambda _ _) -> Vfunc l
    (App name args) -> evalApp env name (map evalE args)
    (If c t f) -> evalIf env c t f
    (Case cond cases) -> evalCase env (evalE cond) cases

note :: Val -> Val -> Val -> Val
note (Vp p _) o d = Vnote $ E.note (valToDur d) (p, valToOct o)
note _ _ _ = error "Note takes a pitch class, an octave and a duration"

rest :: Val -> Val
rest (Vd d _) = Vrest (E.rest (toRational d))
rest (Vn n _) = Vrest (E.rest (toRational n))
rest _ = error "Rests take a duration"

valToDur :: Val -> E.Dur
valToDur (Vd d _) = toRational d
valToDur (Vn n _) = toRational n
valToDur _ = error "Durations must be numeric types"

valToOct :: Val -> E.Octave
valToOct (Vn n _) = fromInteger n
valToOct _ = error "Octaves must be integers"

snippet :: [Val] -> Val
snippet (v@(Vnote _):vs) = case vs of
    [] -> v
    _ -> Vseq v (snippet vs)
snippet (v@(Vrest _):vs) = case vs of
    [] -> v
    _ -> Vseq v (snippet vs)
snippet (_:vs) = error "A snippet should only contain Note or Rest objects"

lookupVar :: Env -> String -> Val
lookupVar env name = fromMaybe (error $ shows name " is undefined." ++ show env) (lookup name env)

evalApp :: Env -> String -> [Val] -> Val
evalApp env name args =
    let (Vfunc (Lambda params body)) = lookupVar env name
        nonexhaust = "Non-exhaustive patterns in function " ++ name
        argenv = concat $ fromMaybe (error nonexhaust) (traverse matchCase (zip params args))
        res = eval (argenv ++ env) body in
    fromMaybe (error $ "Function " ++ name ++ " has no return statement!") (lookup "return" res)

evalIf :: Env -> Expr -> Expr -> Expr -> Val
evalIf env c t f = case evalExpr env c of
    (Vb True) -> evalExpr env t
    (Vb False) -> evalExpr env f
    _ -> error "Breve is not 'truthy'; conditions must evaluate to bool."

evalCase :: Env -> Val -> [(Pat, Expr)] -> Val
evalCase env cond cases =
    -- hlint iterated the following line a good 3-4 times before coming up with
    -- this. Started with "map matchCase $ zip (map fst cases) (repeat c)"
    let envs = map ((\ c -> curry matchCase c cond) . fst) cases
        -- Take [Maybe Env] and [(Pat, Expr)], produce [Maybe (Expr, Env)]
        evals = zipWith (\ m p -> fmap ((,) (snd p)) m) envs cases in
    case msum evals of
        Just (expr, env') -> evalExpr (env' ++ env) expr
        Nothing -> error "Non-exhaustive patterns in Breve case statement."

matchCase :: (Pat, Val) -> Maybe Env
matchCase pv =
    let joinEnv a b = (++) <$> a <*> b in
    case pv of
    (Ppc p, Vp p' _) -> if p == p' then Just [] else Nothing
    (Pn n, Vn n' _) -> if n == n' then Just [] else Nothing
    (Pd d, Vd d' _) -> if d == d' then Just [] else Nothing
    (Pb b, Vb b') -> if b == b' then Just [] else Nothing
    (Pnote p o d, Vnote (E.Prim (E.Note d' (p',o')))) ->
       concat <$> mapM matchCase [(p, Vp p' dummyLoc), (o, Vn (toInteger o') dummyLoc ), (d, Vd (fromRational d') dummyLoc)]
    (Prest d, Vrest (E.Prim (E.Rest d'))) -> matchCase (d, Vd (fromRational d') dummyLoc)
    (Psplit a r, Vlist (v:vs)) -> joinEnv (matchCase (a, v)) (matchCase (r, Vlist vs))
    (Plist (l:ls), Vlist (v:vs)) ->
        if length ls == length vs
        then joinEnv (matchCase (l,v)) (matchCase (Plist ls, Vlist vs))
        else Nothing
    (Plist [], Vlist []) -> Just []
    (Psnip (s:ss), Vseq h t) -> joinEnv (matchCase (s,h)) (matchCase (Psnip ss, t))
    (Psnip (s:[]), Vnote n) -> matchCase(s, Vnote n)
    (Psnip (s:[]), Vrest r) -> matchCase(s, Vrest r)
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
        (Vnote _) -> v
        (Vrest _) -> v
        (Vseq _ _) -> v
        (Vpar _ _) -> v
        _ -> error $ ":+: is undefined for argument " ++ show v

evalBinOp ParOp a b = Vpar (check a) (check b)
    where
    check v = case v of
        (Vnote _) -> v
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
