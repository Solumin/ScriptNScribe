module BreveEval
    -- ( run
    -- , runEnv
    -- , interp
    -- ) where
    where
import BreveLang
import qualified Euterpea (play, line)
import qualified Euterpea.Music.Note.Music as E
import Text.Parsec (runParser)

import Data.Maybe (fromMaybe)
import Data.List (intercalate)

type Music = E.Music E.Pitch

type Binding = (String, Expr)
type Env = [Binding]

-- Takes source code and parses it to generate the AST and parse traces
parse :: String -> (Statement, Traces)
parse input = case runParser breveParser [] "input" input of
    Left err -> error (show err)
    Right st -> st

-- Takes source code and interprets it to generate all of the bindings in a
-- program
interp :: String -> Env
interp input = let (prog,_) = parse input in
    interpStatement [] prog

-- Takes source code and turns it into a single musical phrase
-- eval :: String -> Music
-- eval input =
--     let env = interp input in
--     case lookup "main" env of
--         Just main -> evalMain env main
--         Nothing -> error "No main in program."

-- Takes source code and performs the music described in it
-- perform :: String -> IO()
-- perform = Euterpea.play . eval

parseEval :: String -> Val
parseEval = eval . interp

toMusic :: Val -> Music
toMusic (Ve n) = n
toMusic (Vr r) = r
toMusic (Vs a b) = toMusic a E.:+: toMusic b
toMusic (Vo a b) = toMusic a E.:=: toMusic b

perform :: String -> IO()
perform = Euterpea.play . toMusic . eval . interp

-- ==========
-- Interpret
-- ==========

interpStatement :: Env -> Statement -> Env
interpStatement env (Seq ss) = foldl interpStatement env ss
interpStatement env (Assign v e) = (v, e) : env

-- interpExpr takes the environment and an expression and returns an interpreted
-- expression.
-- For basic Expr, like PitchClass, N, etc., this just returns the Expr.
-- Op expressions interpret their arguments, returning the same op.
-- Notes, Rests, Snippets and Lists have their components interpreted. Type
-- checking will be handled by smart constructors in the eval step.
-- Var expr are looked up in the environment, and throw an error if the
-- expression isn't found.
{-
interpExpr :: Env -> Expr -> Expr
interpExpr env p@(PitchClass _ _) = p
interpExpr env n@(N _ _) = n
interpExpr env d@(D _ _) = d
interpExpr env b@(B _) = b
interpExpr env (UnOpExpr op e) = UnOpExpr op (interpExpr env e)
interpExpr env (BinOpExpr op e1 e2) =
    BinOpExpr op (interpExpr env e1) (interpExpr env e2)
interpExpr env (Note p o d) =
    Note (interpExpr env p) (interpExpr env o) (interpExpr env d)
interpExpr env (Rest d) = Rest (interpExpr env d)
interpExpr env (Snippet ss) = Snippet (map (interpExpr env) ss)
interpExpr env v@(Var _) = v
interpExpr env (List ls) = List (map (interpExpr env) ls)
interpExpr env l@(Lambda _ _) = l
interpExpr env (App s es) = App s (map (interpExpr env) es)
-}

-- ==========
-- Evaluating
-- ==========

-- Attempt 4: Val again (repeat attempt 1, really)

data Val = Vp E.PitchClass
         | Vn Integer | Vd Double
         | Vb Bool
         | Ve Music | Vr Music -- Note and Rest, respectively
         | Vs Val Val -- represents :+: and Snippet
         | Vo Val Val -- represents the :=: operator for rest, notes and snippets
         | Vl [Val]      -- Expr List
         | Vf FunDef
data FunDef = FunDef [String] Env -- lambda eval'd

instance Show Val where
    show (Vp p) = "Val_PitchClass <" ++ shows p ">"
    show (Vn n) = "Val_Integer <" ++ shows n ">"
    show (Vd d) = "Val_Double <" ++ shows d ">"
    show (Vb b) = "Val_Bool <" ++ shows b ">"
    show (Ve e) = "Val_Note <" ++ shows e ">"
    show (Vr r) = "Val_Rest <" ++ shows r ">"
    show (Vs a b) = '(' : shows a " :+: " ++ shows b ")"
    show (Vo a b) = '(' : shows a " :=: " ++ shows b ")"
    show (Vl l) = "Vlist [" ++ intercalate ", " (map show l) ++ "]"
    show (Vf (FunDef args bins)) = "Function <" ++ shows args ">"

instance Eq Val where
    (Vp a) == (Vp b) = a == b
    (Vn a) == (Vn b) = a == b
    (Vn a) == (Vd b) = fromInteger a == b
    (Vd a) == (Vn b) = Vn b == Vd a
    (Vd a) == (Vd b) = a == b
    (Vb a) == (Vb b) = a == b
    (Ve a) == (Ve b) = a == b
    (Vr a) == (Vr b) = a == b
    (Vs a b) == (Vs c d) = a == c && b == d
    (Vo a b) == (Vo c d) = a == c && b == d
    (Vl a) == (Vl b) = a == b
    a == b = error $ unwords ["Cannot compare", show a, "and", show b]

instance Ord Val where
    (Vp a) <= (Vp b) = a <= b
    (Vn a) <= (Vn b) = a <= b
    (Vn a) <= (Vd b) = fromInteger a <= b
    (Vd a) <= (Vn b) = a <= fromInteger b
    (Vd a) <= (Vd b) = a <= b
    (Vb a) <= (Vb b) = a <= b
    (Ve a) <= (Ve b) = a <= b
    (Vr a) <= (Vr b) = a <= b
    (Vo a b) <= (Vo c d) = a <= c && b <= d
    (Vs a b) <= (Vs c d) = a <= c && b <= d
    (Vl a) <= (Vl b) = a <= b
    a <= b = error $ unwords ["Cannot compare", show a, "and", show b]

bindings :: Statement -> Env
bindings (Seq ss) = map bind ss
    where bind (Assign n e) = (n, e)
          bind (Return e) = ("return", e)

eval :: Env -> Val
eval env = evalBinding env "main"

evalBinding :: Env -> String -> Val
evalBinding env name = evalExpr env $ fromMaybe (error $ shows name " is not defined.") (lookup name env)

evalExpr :: Env -> Expr -> Val
evalExpr env expr = let evalE = evalExpr env in
    case expr of
    (PitchClass p _) -> Vp p
    (N n _) -> Vn n
    (D d _) -> Vd d
    (B b) -> Vb b
    (UnOpExpr op e) -> evalUnOp op (evalExpr env e)
    (BinOpExpr op e1 e2) -> evalBinOp op (evalExpr env e1) (evalExpr env e2)
    (Note p o d) -> note (evalE p) (evalE o) (evalE d)
    (Rest d) -> rest (evalE d)
    (Snippet ss) -> snippet (map evalE ss)
    (List ls) -> Vl (map evalE ls)
    (Var s) -> evalE (lookupVar env s)
    (Lambda params body) -> evalFunc params body
    (App name args) -> evalApp env name args
    (If c t f) -> evalIf env c t f

note :: Val -> Val -> Val -> Val
note (Vp p) o d = Ve $ E.note (valToDur d) (p, valToOct o)
note _ _ _ = error "Note takes a pitch class, an octave and a duration"

rest :: Val -> Val
rest (Vd d) = Vr (E.rest (toRational d))
rest (Vn n) = Vr (E.rest (toRational n))
rest _ = error "Rests take a duration"

valToDur :: Val -> E.Dur
valToDur (Vd d) = toRational d
valToDur (Vn n) = toRational n
valToDur _ = error "Durations must be numeric types"

valToOct :: Val -> E.Octave
valToOct (Vn n) = fromInteger n
valToOct _ = error "Octaves must be integers"

snippet :: [Val] -> Val
snippet (v@(Ve _):vs) = case vs of
    [] -> v
    _ -> Vs v (snippet vs)
snippet (v@(Vr _):vs) = case vs of
    [] -> v
    _ -> Vs v (snippet vs)
snippet (_:vs) = error "A snippet should only contain Note or Rest objects"

lookupVar :: Env -> String -> Expr
lookupVar env name = fromMaybe (error $ shows name " is undefined.") (lookup name env)

evalFunc :: [String] -> Statement -> Val
evalFunc params = Vf . FunDef params . bindings

evalApp :: Env -> String -> [Expr] -> Val
evalApp env name args =
    let (Lambda p s) = lookupVar env name
        (Vf (FunDef params binds)) = evalFunc p s in
    evalBinding (binds ++ zip params (map (subst env) args) ++ env) "return"

-- Substitutes any Vars with their expression
subst :: Env -> Expr -> Expr
subst env expr = let sE = subst env in
    case expr of
    (Var s) -> lookupVar env s
    (UnOpExpr op e) -> UnOpExpr op (sE e)
    (BinOpExpr op l r) -> BinOpExpr op (sE l) (sE r)
    (Note p o d) -> Note (sE p) (sE o) (sE d)
    (Rest d) -> Rest (sE d)
    (Snippet ss) -> Snippet (map sE ss)
    (List ls) -> List (map sE ls)
    _ -> expr

evalIf :: Env -> Expr -> Expr -> Expr -> Val
evalIf env c t f = case (evalExpr env c) of
    (Vb True) -> evalExpr env t
    (Vb False) -> evalExpr env f
    _ -> error $ "Breve is not 'truthy'; conditions must evaluate to bool."

evalUnOp :: UnOp -> Val -> Val
evalUnOp Not v = case v of
    (Vb b) -> Vb (not b)
    _ -> error "\"Not\" takes boolean"

evalBinOp :: BinOp -> Val -> Val -> Val

evalBinOp Add (Vd d1) (Vd d2) = Vd (d1 + d2)
evalBinOp Add (Vd d1) (Vn n2) = Vd (d1 + fromInteger n2)
evalBinOp Add (Vn n1) (Vd d2) = Vd (fromInteger n1 + d2)
evalBinOp Add (Vn n1) (Vn n2) = Vn (n1 + n2)
-- Using the behavior of Euterpea's pitch and trans functions, which always
-- return a sharp note instead of a flat enharmonic.
evalBinOp Add (Vp pc) (Vn n) = Vp (pitches !! ((E.pcToInt pc + d) `mod` 12))
    where
        pitches = [E.C, E.Cs, E.D, E.Ds, E.E, E.F, E.Fs, E.G, E.Gs, E.A, E.As, E.B]
        d = fromInteger n

evalBinOp Sub (Vd d1) (Vd d2) = Vd (d1 - d2)
evalBinOp Sub (Vd d1) (Vn n2) = Vd (d1 - fromInteger n2)
evalBinOp Sub (Vn n1) (Vd d2) = Vd (fromInteger n1 - d2)
evalBinOp Sub (Vn n1) (Vn n2) = Vn (n1 - n2)
evalBinOp Sub (Vp pc) (Vn n) = evalBinOp Add (Vp pc) (Vn (negate n))

evalBinOp Mult (Vd d1) (Vd d2) = Vd (d1 * d2)
evalBinOp Mult (Vd d1) (Vn n2) = Vd (d1 * fromInteger n2)
evalBinOp Mult (Vn n1) (Vd d2) = Vd (fromInteger n1 * d2)
evalBinOp Mult (Vn n1) (Vn n2) = Vn (n1 * n2)

-- Note that Div always returns a double!!
evalBinOp Div (Vd d1) (Vd d2) = Vd (d1 / d2)
evalBinOp Div (Vd d1) (Vn n2) = Vd (d1 / fromInteger n2)
evalBinOp Div (Vn n1) (Vd d2) = Vd (fromInteger n1 / d2)
evalBinOp Div (Vn n1) (Vn n2) = Vd (fromInteger n1 / fromInteger n2)

evalBinOp SeqOp a b = Vs (check a) (check b)
    where
    check v = case v of
        (Ve _) -> v
        (Vr _) -> v
        (Vs _ _) -> v
        (Vo _ _) -> v
        _ -> error $ ":+: is undefined for argument " ++ show v

evalBinOp ParOp a b = Vo (check a) (check b)
    where
    check v = case v of
        (Ve _) -> v
        (Vr _) -> v
        (Vs _ _) -> v
        (Vo _ _) -> v
        _ -> error $ ":=: is undefined for argument " ++ show v

evalBinOp Eq  a b = Vb (a == b)
evalBinOp Neq a b = Vb (a /= b)
evalBinOp Lt  a b = Vb (a <  b)
evalBinOp Lte a b = Vb (a <= b)
evalBinOp Gt  a b = Vb (a >  b)
evalBinOp Gte a b = Vb (a >= b)

evalBinOp o a b = error $ unwords ["Op", show o, "is undefined for args", show a, show b]
