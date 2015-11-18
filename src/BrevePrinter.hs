module BrevePrinter where
import BreveLang
import Data.List (intercalate)
import qualified Euterpea.Music.Note.Music as E

{-
 - The opposite of parse: Takes statements and prints out a program.
 - The name is a reference to "unwords" and "unlines"
 -}

unparse :: Statement -> String
unparse (Seq ss) = intercalate (";\n") $ map unparse ss
unparse (v := e) = unwords [v, ":=", unparseExpr e]

unparseExpr :: Expr -> String
unparseExpr (PitchClass p _) = show p
unparseExpr (Octave o _) = show o
unparseExpr (Duration d _) = durToStr d
unparseExpr (Note p o d) = '(' : unwords (map unparseExpr [p,o,d]) ++ ")"
unparseExpr (Rest d) = concat ["(", " rest", unparseExpr d, " )"]
unparseExpr (Snippet ss) = '{' : intercalate ", " (map unparseExpr ss) ++ "}"
unparseExpr (Var s) = s

durToStr :: E.Dur -> String
durToStr d
    | d == E.bn = "bn"
    | d == E.wn = "wn"
    | d == E.hn = "hn"
    | d == E.qn = "qn"
    | d == E.en = "en"
    | d == E.sn = "sn"
    | d == E.sfn = "sfn"
    | d == E.tn = "tn"
    | d == E.dwn = "dwn"
    | d == E.dhn = "dhn"
    | d == E.dqn = "dqn"
    | d == E.den = "den"
    | d == E.dsn = "dsn"
    | d == E.dtn = "dtn"
    | d == E.ddhn = "ddhn"
    | d == E.ddqn = "ddqn"
    | d == E.dden = "dden"
