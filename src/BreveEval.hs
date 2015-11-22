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
eval input = case lookup "main" (interp input) of
    Just m -> makeMusic m
    Nothing -> error "No main in program."

-- Takes source code and performs the music described in it
perform :: String -> IO()
perform = Euterpea.play . eval

-- interpStatement _ _ = [("main", N 5 (0,0))]
makeMusic _ = (E.f 4 E.wn)

-- ========
-- Interpret
-- ========

interpStatement :: Env -> Statement -> Env
interpStatement env (Seq ss) = foldl interpStatement env ss
interpStatement env (Assign v e) = (v, interpExpr env e) : env

interpExpr :: Env -> Expr -> Expr
interpExpr env (Note p o d) = note (interpExpr env p) (interpExpr env o) (interpExpr env d)
interpExpr env (Rest d) = rest (interpExpr env d)
interpExpr env (Snippet ss) = snippet (map (interpExpr env) ss)
interpExpr env (Var v) = case lookup v env of
    Just e -> e
    Nothing -> error ("Uknown variable " ++ v)
interpExpr env (List ls) = List (map (interpExpr env) ls)

note :: Expr -> Expr -> Expr -> Expr
note pc@(PitchClass _ _) o@(N _ _) d@(D _ _) = Note pc o d
note _ _ _ = error "Note takes a pitch class, an octave and a duration"

rest :: Expr -> Expr
rest d@(D _ _) = Rest d

snippet :: [Expr] -> Expr
snippet = snippet' (Snippet [])

snippet' :: Expr -> [Expr] -> Expr
snippet' (Snippet ss) ((n@(Note _ _ _)):ns) = snippet' (Snippet (ss ++ [n])) ns
snippet' (Snippet ss) ((r@(Rest _)):rs) = snippet' (Snippet (ss ++ [r])) rs
snippet' (Snippet ss) _ = error "Snippet takes only Notes and Rests"

{-

type Binding = (String, Music)
type Env = [Binding]

-- Take a program and produce the AST and traces
interp :: String -> (Statement, Traces)
interp input = case runParser breveParser [] "input" input of
    Left err -> error (show err)
    Right st -> st

-- Take a program and perform it
run :: String -> IO ()
run s = runEnv s []

runEnv :: String -> Env -> IO ()
runEnv input env = let (prog,_) = interp input in
    case lookup "main" (eval env prog) of
        Just m ->  E.play m
        Nothing -> putStrLn "<No main>"

-- run :: Statement -> Env -> IO()
-- run s env = case lookup "main" (eval env s) of
--     Just m ->  putStrLn (show m) >> putStrLn (show t) >> E.play m
--     Nothing -> putStrLn (show s) >> putStrLn (show t)

eval :: Env -> Statement -> Env
eval env (Seq ss) = foldl (evalAssign) env ss
eval env s = evalAssign env s

evalAssign :: Env -> Statement -> Env
evalAssign env (v := e) = (v, evalExpr env e) : env

evalExpr :: Env -> Expr -> Music
evalExpr env n@(Note _ _ _) = evalNote n
evalExpr env r@(Rest _) = evalRest r
evalExpr env s@(Snippet _) = evalSnippet s
-- evalExpr env (s1 :=: s2) = evalExpr env s1 E.:=: evalExpr env s2
-- evalExpr env (s1 :+: s2) = evalExpr env s1 E.:+: evalExpr env s2
evalExpr env (Var v) = case lookup v env of
    Just m -> m
    Nothing -> error ("Unknown variable " ++ v)

evalNote :: Expr -> Music
evalNote (Note (PitchClass p _) (Octave o _) (Duration d _)) = E.note d (p, o)

evalRest :: Expr -> Music
evalRest (Rest (Duration d _)) = E.rest d

evalSnippet :: Expr -> Music
evalSnippet (Snippet ss) = E.line (map body ss)
    where
        body :: Expr -> Music
        body n@(Note p o d) = evalNote n
        body r@(Rest d) = evalRest r
        body e = error ("Snippet can only contain Notes and Rests, received " ++ (show e))
-}
