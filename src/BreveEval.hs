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
eval :: String -> Music
eval input =
    let env = interp input in
    case lookup "main" env of
        Just main -> let (Vm m) = evalExpr env main in m
        Nothing -> error "No main in program."

-- Takes source code and performs the music described in it
perform :: String -> IO()
perform = Euterpea.play . eval

-- ==========
-- Interpret
-- ==========

interpStatement :: Env -> Statement -> Env
interpStatement env (Seq ss) = foldl interpStatement env ss
interpStatement env (Assign v e) = (v, interpExpr env e) : env

-- interpExpr takes the environment and an expression and returns an interpreted
-- expression.
-- For basic Expr, like PitchClass, N, etc., this just returns the Expr.
-- Op expressions interpret their arguments, returning the same op.
-- Notes, Rests, Snippets and Lists have their components interpreted. Type
-- checking will be handled by smart constructors in the eval step.
-- Var expr are looked up in the environment, and throw an error if the
-- expression isn't found.
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
interpExpr env (Var v) = case lookup v env of
    Just e -> e
    Nothing -> error ("Uknown variable " ++ v)
interpExpr env (List ls) = List (map (interpExpr env) ls)

-- ==========
-- Evaluating
-- ==========

-- Main can validly be a note, a rest or a snippet.
-- Notes and Rests are lifted to Snippets automatically.
-- evalMain :: Env -> Expr -> Music
-- evalMain env n@(Note _ _ _) = evalMain env (Snippet [n])
-- evalMain env r@(Rest _) = evalMain env (Snippet [r])
-- evalMain env s@(Snippet ss) = Euterpea.line (map (evalExpr env) ss)
-- evalMain _ _ = error "Main must be a Snippet (or a Note or Rest)"

data Val = Vp E.PitchClass | Vn Integer | Vd Double | Vb Bool | Vm Music | VList [Val]

evalExpr :: Env -> Expr -> Val
evalExpr env (PitchClass p _) = Vp p
evalExpr env (N n _) = Vn n
evalExpr env (D d _) = Vd d
evalExpr env (B b) = Vb b
evalExpr env (UnOpExpr op e) = evalUnOp op (evalExpr env e)
evalExpr env (BinOpExpr op e1 e2) =
    evalBinOp op (evalExpr env e1) (evalExpr env e2)
evalExpr env (Note p o d) =
    note (evalExpr env p) (evalExpr env o) (evalExpr env d)
evalExpr env (Rest d) = rest (evalExpr env d)
evalExpr env (Snippet ss) = Vm $ line (map (evalExpr env) ss)
evalExpr env (Var v) = case lookup v env of
    Just e -> evalExpr env e
    Nothing -> error ("Unknown variable " ++ v)
evalExpr env (List ls) = VList (map (evalExpr env) ls)

evalUnOp :: UnOp -> Val -> Val
evalUnOp Not (Vb b) = Vb (not b)
evalUnOp Not _ = error "Not takes boolean"

evalBinOp :: BinOp -> Val -> Val -> Val
evalBinOp Add (Vd d1) (Vd d2) = Vd (d1 + d2)
evalBinOp Add (Vn n1) (Vn n2) = Vn (n1 + n2)

evalBinOp Sub (Vd d1) (Vd d2) = Vd (d1 - d2)
evalBinOp Sub (Vn n1) (Vn n2) = Vn (n1 - n2)

evalBinOp Mult (Vd d1) (Vd d2) = Vd (d1 * d2)
evalBinOp Mult (Vn n1) (Vn n2) = Vn (n1 * n2)

-- Note that Div always returns a double!!
evalBinOp Div (Vd d1) (Vd d2) = Vd (d1 / d2)
evalBinOp Div (Vn n1) (Vn n2) = Vd ((fromInteger n1) / (fromInteger n2))

evalBinOp SeqOp (Vm m1) (Vm m2) = Vm (m1 E.:+: m2)
evalBinOp ParOp (Vm m1) (Vm m2) = Vm (m1 E.:=: m2)

evalBinOp Eq  (Vb b1) (Vb b2) = Vb (b1 == b2)
evalBinOp Neq (Vb b1) (Vb b2) = Vb (b1 /= b2)
evalBinOp Lt  (Vb b1) (Vb b2) = Vb (b1 <  b2)
evalBinOp Lte (Vb b1) (Vb b2) = Vb (b1 <= b2)
evalBinOp Gt  (Vb b1) (Vb b2) = Vb (b1 >  b2)
evalBinOp Gte (Vb b1) (Vb b2) = Vb (b1 >= b2)

note :: Val -> Val -> Val -> Val
note (Vp p) o d = Vm $ E.note (valToDur d) (p, (valToOct o))
note _ _ _ = error "Note takes a pitch class, an octave and a duration"

rest :: Val -> Val
rest (Vd d) = Vm (E.rest (toRational d))
rest (Vn n) = Vm (E.rest (toRational n))
rest _ = error "Rests take a duration"

valToDur :: Val -> E.Dur
valToDur (Vd d) = toRational d
valToDur (Vn n) = toRational n
valToDur _ = error "Durations must be numeric types"

valToOct :: Val -> E.Octave
valToOct (Vn n) = fromInteger n
valToOct _ = error "Octaves must be integers"

line :: [Val] -> Music
line [] = E.rest 0
line [(Vm m)] = m
line ((Vm m):vs) = m E.:+: (line vs)
line _ = error "Expected music in the snippet"
