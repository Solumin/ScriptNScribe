import BreveLang
import qualified Euterpea.Music.Note.Music as E hiding (Note)
import qualified Euterpea as E (play, line)
import Text.Parsec (parse)

type Music = E.Music E.Pitch

eval :: Statement -> Music
eval (Seq (s:ss)) = evalStatement s

evalStatement :: Statement -> Music
evalStatement (v := n) = evalNote n

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
        body _ = error "Snippet can only contain Notes and Rests"


parseEval :: String -> IO()
parseEval input = case parse breveParser "input" input of
    Left err -> error (show err)
    Right statement -> E.play (eval statement)
