import BreveLang
import qualified Euterpea.Music.Note.Music as E hiding (Note)
import qualified Euterpea as E (play, line)
import Text.Parsec (runParser)

type Music = E.Music E.Pitch

type Binding = (String, Music)
type Env = [Binding]

parseEval :: String -> IO()
parseEval input = case runParser breveParser [] "input" input of
    Left err -> error (show err)
    Right statement -> run statement

run :: (Statement, Traces) -> IO()
run (s, t) = case lookup "main" (eval [] s) of
    Just m ->  putStrLn (show m) >> E.play m
    Nothing -> putStrLn (show s ++ show t)

eval :: Env -> Statement -> Env
eval env (Seq ss) = foldl (evalStatement) env ss
eval env s = evalStatement env s

evalStatement :: Env -> Statement -> Env
evalStatement env (v := e) = (v, evalExpr env e) : env

evalExpr :: Env -> Expr -> Music
evalExpr env n@(Note _ _ _) = evalNote n
evalExpr env r@(Rest _) = evalRest r
evalExpr env s@(Snippet _) = evalSnippet s
evalExpr env (Var v) = case lookup v env of
    Just m -> m
    Nothing -> error ("Unknown variable " ++ v)

evalNote :: Expr -> Music
evalNote (Note (PitchClass p) (Octave o) (Duration d)) = E.note d (p, o)

evalRest :: Expr -> Music
evalRest (Rest (Duration d)) = E.rest d

evalSnippet :: Expr -> Music
evalSnippet (Snippet ss) = E.line (map body ss)
    where
        body :: Expr -> Music
        body n@(Note p o d) = evalNote n
        body r@(Rest d) = evalRest r
        body e = error ("Snippet can only contain Notes and Rests, received " ++ (show e))
