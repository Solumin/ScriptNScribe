module BreveEval
    ( run
    , runEnv
    , interp
    ) where
import BreveLang
import qualified Euterpea.Music.Note.Music as E hiding (Note)
import qualified Euterpea as E (play, line)
import Text.Parsec (runParser)

type Music = E.Music E.Pitch

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
eval env (Seq ss) = foldl (evalStatement) env ss
eval env s = evalStatement env s

evalStatement :: Env -> Statement -> Env
evalStatement env (v := e) = (v, evalExpr env e) : env

evalExpr :: Env -> Expr -> Music
evalExpr env n@(Note _ _ _) = evalNote n
evalExpr env r@(Rest _) = evalRest r
evalExpr env s@(Snippet _) = evalSnippet s
evalExpr env (s1 :=: s2) = evalExpr env s1 E.:=: evalExpr env s2
evalExpr env (s1 :+: s2) = evalExpr env s1 E.:+: evalExpr env s2
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
