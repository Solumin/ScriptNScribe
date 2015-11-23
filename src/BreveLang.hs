module BreveLang
    where
    -- (
    --   Expr (..)
    -- , Statement (..)
    -- , Trace
    -- , Traces
    -- , breveParser
    -- , pitchClasses
    -- , durations
    -- ) where
{-
Breve:
pitchclass ::= A | B ...
octave ::= digit { digit }*
note ::= ( pitchclass octave duration )
rest ::= ( "rest" duration )
snippet ::= '{' note {, rest }* '}'
var ::= letter { letter | digit }*
list = "[" expr[, expr]* "]"

expr ::= note | rest | list | snippet | expr duop expr
statement ::= var = expr | var = snippet | statement {; statement}*

lineComment ::= --
blockComment ::= {- ... -}

Example program:
snippet1 = {(d 4 qn), (fs 4 qn), (a 4 qn)};
snippet2 = {(d 4 wn)} :=: [(fs 4 wn)] :=: [(a 4 wn)]
main = snippet1 :+: {(r qn)} :+: snippet

Output:
(music) D major arpeggio in quarter notes, a beat of rest, D major chord
(score)
    (main) Treble cleff, with 3 quarter notes stacked: D F# A
    (snippet 1) Treble cleff, with 3...
-}
import qualified Euterpea.Music.Note.Music as E
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token

-- Poor man's map
type Loc = (Int, Int)
type Traces = [Expr]

type Parser = Parsec String Traces

data Expr = PitchClass E.PitchClass Loc
          | N Integer Loc | D Double Loc
          | B Bool
          | UnOpExpr UnOp Expr
          | BinOpExpr BinOp Expr Expr
          | Note Expr Expr Expr -- PitchClass, Octave, Duration
          | Rest Expr           -- Duration
          | Snippet [Expr]      -- Note | Rest
          | Var String
          | List [Expr]         -- Homogeneous
          deriving (Eq)

instance Show Expr where
    show (PitchClass e _) = show e
    show (N i _) = show i
    show (D d _) = show d
    show (B b) = show b
    show (UnOpExpr u e) = shows u (show e)
    show (BinOpExpr b@Div e1 e2) = concat [show e1, show b, show e2]
    show (BinOpExpr b e1 e2) = unwords [show e1, show b, show e2]
    show (Note p o d) = '(' : unwords [show p, show o, shows d ")"]
    show (Rest d) = "(rest " ++ shows d ")"
    show (Snippet ss) = '{' : intercalate ", " (map show ss) ++ "}"
    show (Var v) = v
    show (List ls) = '[' : intercalate ", " (map show ls) ++ "]"

data BinOp =
      SeqOp | ParOp                     -- snippets
    | Add | Mult | Div | Sub            -- math
    | Eq | Neq | Lt | Lte | Gt | Gte    -- equality
    deriving (Eq)
instance Show BinOp where
    show SeqOp = ":+:"
    show ParOp = ":=:"
    show Add = "+"
    show Mult = "*"
    show Div = "/"
    show Sub = "-"
    show Eq = "=="
    show Neq = "!="
    show Lt = "<"
    show Lte = "<="
    show Gt = ">"
    show Gte = ">="

data UnOp = Not deriving (Eq)
instance Show UnOp where
    show Not = "!"

data Statement = Assign String Expr | Seq [Statement] deriving (Eq)
instance Show Statement where
    show (Assign s e) = unwords [s, "=", shows e ";"]
    show (Seq ss) = unlines (map show ss)

pitchClasses = [n : m | n <- ['A'..'G'], m <- ["ff", "ss", "f", "s", ""]]
keywords = ["rest", "true", "false", "if", "else", "def"]

mathOps = ["+", "-", "/", "*"]
boolOps = ["==", "<", "<=", ">", ">="]
catOps = ["=", ":=:", ":+:"]

breveDef :: LanguageDef st
breveDef = emptyDef { commentStart = "{-"
                    , commentEnd = "-}"
                    , nestedComments = True
                    , commentLine = "--"
                    , identStart = lower <|> char '_'
                    , identLetter = alphaNum <|> char '_' <|> char '-'
                    , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                    , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                    , reservedNames = pitchClasses ++ keywords
                    , reservedOpNames = catOps ++ mathOps ++ boolOps
                    , caseSensitive = True
                    }

-- extract the parsers we need
TokenParser { identifier = b_identifier
            , reserved = b_reserved
            , reservedOp = b_resop
            , naturalOrFloat = b_number
            , symbol = b_symbol
            , parens = b_parens
            , braces = b_braces
            , brackets = b_brackets
            , semi = b_semi
            , commaSep = b_commaSep
            , whiteSpace = b_whitespace } = makeTokenParser breveDef
-- The default semiSep1 uses sepBy, which expects something like "a, b, c".
-- That's fine for lists, but we're processing semicolon-terminated statements.
-- We expect the last statement to have a semicolon! So sepEndBy makes more
-- sense. (It optionally eats the last separator)
b_semiSep1 p = sepEndBy p b_semi

breveParser :: Parser (Statement, Traces)
breveParser = do
    p <- b_whitespace >> fmap Seq (b_semiSep1 parseStatement) <* eof
    s <- getState
    return (p, s)

-- ===================
-- Parsing Statements
-- ===================

parseStatement :: Parser Statement
parseStatement = try parseAssign

parseAssign :: Parser Statement
parseAssign = do
    v <- b_identifier
    b_resop "="
    e <- parseExpr
    return (Assign v e)

-- ===================
-- Parsing Expressions
-- ===================

parseExpr :: Parser Expr
parseExpr = buildExpressionParser opTable term <?> msg
    where
        term = parseTerm <|> b_parens parseExpr <?> msg
        msg = "an expression or operation (the statement ended early!)"

opTable = [ [ inf ParOp AssocRight, inf SeqOp AssocRight]
          , [ pref Not]
          , [ math Mult, math Div]
          , [ math Add, math Sub]
          , [ bool Lt, bool Lte, bool Gt, bool Gte]
          , [ bool Eq, bool Neq]
          ]
    where
        pref op = Prefix (b_resop (show op) *> return (UnOpExpr op))
        inf op = Infix (b_resop (show op) *> return (BinOpExpr op))
        math op = inf op AssocLeft
        bool = math -- same structure, just differentiate in the table

parseTerm :: Parser Expr
parseTerm = try parseNote
        <|> try parseRest
        <|> parseSnippet
        <|> parseList
        <|> parseNum
        <|> parsePitchClass
        <|> parseVar
        <|> parseBool

parseNote :: Parser Expr
parseNote = b_parens (Note <$> parseExpr <*> parseExpr <*> parseExpr)

parseRest :: Parser Expr
parseRest = Rest <$> b_parens (b_reserved "rest" *> parseExpr)

parseSnippet :: Parser Expr
parseSnippet = Snippet <$> b_braces (b_commaSep (try parseNote <|> try parseRest <?> msg))
    where msg = "Note or Rest"

parseVar :: Parser Expr
parseVar = Var <$> b_identifier

parsePitchClass :: Parser Expr
parsePitchClass = do
    pc <- parser
    loc <- getLoc
    let pcc = PitchClass (read pc) loc
    addState pcc
    return pcc
    where
        parser = choice (map (try . b_symbol) pitchClasses) <?> msg
        msg = "capitol letter A-G, possibly followed by ff, f, s or ss"

parseList :: Parser Expr
parseList = List <$> b_brackets (b_commaSep parseExpr)

parseBool :: Parser Expr
parseBool = parseTrue <|> parseFalse
    where
    parseTrue = b_reserved "true" *> return (B True) <?> "true"
    parseFalse = b_reserved "false" *> return (B False) <?> "false"

parseNum :: Parser Expr
parseNum = do
    p <- b_number
    res <- case p of
        Left i -> N i <$> getLoc
        Right d -> D d <$> getLoc
    addState res
    return res

-- ============
-- Utility
-- ============

getLoc :: Parser (Int,Int)
getLoc = do
    pos <- getPosition
    let loc = (sourceLine pos, sourceColumn pos)
    -- modifyState ((:) (e, loc)
    return loc

addState :: Expr -> Parser ()
addState s = do
    modifyState ((:) s)
    return ()

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
    | otherwise = show d
