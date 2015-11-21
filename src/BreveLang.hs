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
duration ::= qn | wn ...
octave ::= digit { digit }*
note ::= ( pitchclass octave duration )
rest ::= ( "rest" duration )
snippet ::= '{' note {, rest }* '}'
var ::= letter { letter | digit }*
list = [ pitchclass | duration | octave | note | rest | snippet | var ]

expr ::= note | rest | list | snippet | expr duop expr
statement ::= var := expr | var := snippet | statement {; statement}*

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
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.Token

-- Poor man's map
type Loc = (Int, Int)
type Traces = [Expr]

type Parser = Parsec String Traces

data Expr = PitchClass E.PitchClass Loc
          | Octave E.Octave Loc
          | Duration E.Dur Loc
          | N Int Loc | R Rational Loc
          | B Bool
          | Note Expr Expr Expr -- PitchClass, Octave, Duration
          | Rest Expr           -- Duration
          | Snippet [Expr]      -- Note | Rest
          | Expr :=: Expr -- Snippet :=: Snippet
          | Expr :+: Expr
          | Var String
          -- | List [Expr]         -- Homogeneous
          deriving (Show, Eq)
data Statement = String := Expr | Seq [Statement] deriving (Show, Eq)

pitchClasses = [n : m | n <- ['A'..'G'], m <- ["ff", "ss", "f", "s", ""]]
durations = ["bn","wn","hn","qn","en","sn","sfn","tn","dwn","dhn","dqn","den",
    "dsn","dtn", "ddhn","ddqn","dden"]
keywords = ["rest", "true", "false", "if", "else", "def"]

breveDef :: LanguageDef st
breveDef = emptyDef { commentStart = "{-"
                    , commentEnd = "-}"
                    , nestedComments = True
                    , commentLine = "--"
                    , identStart = lower <|> char '_'
                    , identLetter = alphaNum <|> char '_' <|> char '-'
                    , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                    , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                    , reservedNames = pitchClasses ++ durations ++ keywords
                    , reservedOpNames = ["=", "+", "-", "*", "/", ":=:", ":+:"]
                    , caseSensitive = True
                    }

-- extract the parsers we need
TokenParser { identifier = b_identifier
            , reserved = b_reserved -- maybe separator into own parsers, natch?
            , operator = b_op
            , reservedOp = b_resop
            , stringLiteral = b_stringLit
            , integer = b_int
            , natural = b_natural
            , symbol = b_symbol
            , lexeme = b_lexeme -- perhaps using this instead of reserved.
            , parens = b_parens
            , braces = b_braces
            , brackets = b_brackets
            , semi = b_semi
            -- , semiSep1 = b_semiSep1
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
    return (v := e)

-- ===================
-- Parsing Expressions
-- ===================

parseExpr :: Parser Expr
parseExpr = try parseNote
        <|> try parseRest
        <|> parseSnippetOp
        <|> parseSnippet
        <|> parsePitchClass
        <|> parseOctave
        <|> parseDuration
        <|> parseVar
        <?> "an expression"

parseNote :: Parser Expr
parseNote = b_parens (do
    pc <- parsePitchClass
    o <- parseOctave
    dur <- parseDuration
    return (Note pc o dur))

parseRest :: Parser Expr
parseRest = Rest <$> b_parens (b_reserved "rest" *> parseDuration)

parseSnippet :: Parser Expr
parseSnippet = Snippet <$> b_braces (b_commaSep (try parseNote <|> try parseRest <?> msg))
    where msg = "Note or Rest"

parseVar :: Parser Expr
parseVar = Var <$> b_identifier

parsePitchClass :: Parser Expr
-- parsePitchClass = (PitchClass <$> (read <$> parser) <*> getLoc) >>= (\e -> addState e *> return e)
parsePitchClass = do
    pc <- parser
    loc <- getLoc
    let pcc = PitchClass (read pc) loc
    addState pcc
    return pcc
    where
        parser = choice (map (try . b_symbol) pitchClasses) <?> msg
        msg = "capitol letter A-G, possibly followed by ff, f, s or ss"

parseOctave :: Parser Expr
-- parseOctave = fmap (Octave . fromInteger) parser
parseOctave = do
    o <- parser
    loc <- getLoc
    let oct = Octave (fromInteger o) loc
    addState oct
    return oct
    where parser = b_natural <?> "natural number (0 - 8, most likely)"

parseDuration :: Parser Expr
-- parseDuration = fmap (Duration . strToDur) parser
parseDuration = do
    d <- parser
    loc <- getLoc
    let dur = Duration (strToDur d) loc
    addState dur
    return dur
    where parser = choice (map (try . b_symbol) durations) <?> msg
          msg = "duration (e.g. qn)"

strToDur :: String -> E.Dur
strToDur s = case s of
    "bn"   -> E.bn
    "wn"   -> E.wn
    "hn"   -> E.hn
    "qn"   -> E.qn
    "en"   -> E.en
    "sn"   -> E.sn
    "sfn"  -> E.sfn
    "tn"   -> E.tn
    "dwn"  -> E.dwn
    "dhn"  -> E.dhn
    "dqn"  -> E.dqn
    "den"  -> E.den
    "dsn"  -> E.dsn
    "dtn"  -> E.dtn
    "ddhn" -> E.ddhn
    "ddqn" -> E.ddqn
    "dden" -> E.dden

-- ===========
-- Snippet Expr
-- ===========

parseSnippetOp :: Parser Expr
parseSnippetOp = buildExpressionParser snippetTable (parseSnippet <?> "a snippet: {Note | Rest}") <?> "snippet op"
snippetTable = [[ Infix (b_resop ":=:" *> return (:=:)) AssocRight
                , Infix (b_resop ":+:" *> return (:+:)) AssocRight ]]

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
