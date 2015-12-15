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
integer ::= (-) digit (digit)*
float ::= (-) integer '.' digit (digit)*
boolean ::= 'true' | 'false'
note = '(' expr expr expr ')'
rest ::= '(' "rest" expr ')'
snippet ::= '{' expr (',' expr )* '}'
identifier ::= (lowercase | _ ) (letter | digit | underscore)*
var ::= identifier
list = '[' expr{',' expr}* ']'

expr ::= pitchclass
       | interger | float | boolean
       | note | rest
       | snippet | list
       | '(\' ident* '->' (expr | (assign* return) ')'
       | identifier'('expr*')'
       | 'if' expr 'then' expr 'else' expr
       | 'case' expr 'of' (pattern '->' expr ';')

assign ::= identifier = expr
return ::= 'return' expr
sequence ::= statement (';' statement)*
statement ::= assign | return | sequence

pattern ::= pitchclass
          | integer
          | float
          | boolean
          | '(' pattern pattern pattern ')'
          | '(' 'rest' pattern ')'
          | '[' pat (',' pat)* ']'
          | '[' ']'
          | '(' pat ':' pat (':' pat)* ')'
          | '{' pat (',' pat)* '}'
          | '(' pat ':' '{' '}' ')'
          | identifier
          | '_'

lineComment ::= --
blockComment ::= {- ... -}

-}
import qualified Euterpea.Music.Note.Music as E
import Data.List (intercalate)
import Text.Parsec
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import Text.Parsec.String (Parser)
import Text.Parsec.Token

type Loc = (Int, Int)

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
          | Lambda [Pat] Statement    -- Seq or Return, most likely
          | App String [Expr]   -- Function name, arguments
          | If Expr Expr Expr  -- If statement: condition, true branch, false branch
          | Case Expr [(Pat, Expr)] -- case expr of pat -> expr;...
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
    show (Lambda v s) = '(':'\\': unwords (map show v) ++ " -> " ++ shows s ")"
    show (App n as) = n ++ "(" ++ intercalate ", " (map show as) ++ ")"
    show (If c t f) = "if " ++ shows c " then " ++ shows t " else " ++ show f
    show (Case e ps) = "case " ++ shows e " of " ++
                       intercalate "; " (map (\(p,r) -> '(' : show p ++ " -> " ++ shows r ")") ps)

data BinOp =
      SeqOp | ParOp                     -- snippets
    | Add | Mult | Div | Sub            -- math
    | Eq | Neq | Lt | Lte | Gt | Gte    -- equality
    | Cons | Cat                        -- list
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
    show Cons = ":"
    show Cat = "++"

instance Read BinOp where
    readsPrec _ (':':s) = case s of
        ('+':':':s') -> [(SeqOp, s')]
        ('=':':':s') -> [(ParOp, s')]
        _ -> [(Cons, s)]
    readsPrec _ ('+':s) = case s of
        ('+':s') -> [(Cat, s')]
        _ -> [(Add, s)]
    readsPrec _ ('*':s) = [(Mult, s)]
    readsPrec _ ('/':s) = [(Div, s)]
    readsPrec _ ('-':s) = [(Sub, s)]
    readsPrec _ ('!':'=':s) = [(Neq, s)]
    readsPrec _ ('=':'=':s) = [(Eq, s)]
    readsPrec _ ('<':s) = case s of
        ('=':s') -> [(Lte, s')]
        _ -> [(Lt, s)]
    readsPrec _ ('>':s) = case s of
        ('=':s') -> [(Gte, s')]
        _ -> [(Gt, s)]

data UnOp = Not | Neg deriving (Eq)
instance Show UnOp where
    show Not = "!"
    show Neg = "-"

instance Read UnOp where
    readsPrec _ ('!':s) = [(Not, s)]
    readsPrec _ ('-':s) = [(Neg, s)]

data Statement = Assign String Expr | Return Expr | Seq [Statement] deriving (Eq)
instance Show Statement where
    show (Assign s e) = unwords [s, "=", shows e ";"]
    show (Seq ss) = unlines (map show ss)
    show (Return e) = "return " ++ shows e ";"

data Pat =
    -- Constants
      Ppc E.PitchClass
    | Pn Integer
    | Pd Double
    | Pb Bool
      -- Structures
    | Pnote Pat Pat Pat
    | Prest Pat
    | Plist [Pat]
    | Psnip [Pat]
      -- Variables
    | Pvar String
    | Pwc -- wildcard
    | Ppat String Pat -- at pattern, e.g. l@(x:xs)
      -- Splitting
    | Psplit Pat Pat -- (:) for splitting list elements off
    deriving (Show, Eq)

pitchClasses = [n : m | n <- ['A'..'G'], m <- ["ff", "ss", "f", "s", ""]]
keywords = ["rest", "true", "false", "if", "then", "else", "def", "return", "case", "of"]

mathOps = map show [Add, Sub, Mult, Div]
boolOps = map show [Eq, Neq, Lt, Lte, Gt, Gte]
listOps = map show [Cons, Cat]
catOps = map show [SeqOp, ParOp] ++ ["=", "\\", "->", "@"]

breveDef :: LanguageDef st
breveDef = emptyDef { commentStart = "{-"
                    , commentEnd = "-}"
                    , nestedComments = True
                    , commentLine = "--"
                    , identStart = lower <|> char '_'
                    , identLetter = alphaNum <|> char '_'
                    , opStart = oneOf ":!#$%&*+./<=>?@\\^|-~"
                    , opLetter = oneOf ":!#$%&*+./<=>?@\\^|-~"
                    , reservedNames = pitchClasses ++ keywords
                    , reservedOpNames = catOps ++ mathOps ++ boolOps ++ listOps
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
-- sense. (The final separator *is* optional.)
b_semiSep1 p = sepEndBy p b_semi

breveParser :: Parser Statement
breveParser = b_whitespace >> parseSeq <* eof

-- ===================
-- Parsing Statements
-- ===================

parseSeq :: Parser Statement
parseSeq = fmap Seq (b_semiSep1 parseStatement)

parseStatement :: Parser Statement
parseStatement = parseReturn <|> try parseAssign

parseAssign :: Parser Statement
parseAssign = do
    v <- b_identifier
    b_resop "="
    e <- parseExpr
    return (Assign v e)

parseReturn :: Parser Statement
parseReturn = Return <$> (b_reserved "return" *> parseExpr)

-- ===================
-- Parsing Expressions
-- ===================

parseExpr :: Parser Expr
parseExpr = buildExpressionParser opTable term <?> msg
    where
        term = parseTerm <|> b_parens parseExpr <?> msg
        msg = "an expression or operation (the statement ended early!)"

opTable = [ [ inf ParOp AssocRight, inf SeqOp AssocRight]
          , [ pref Not, pref Neg]
          , [ math Mult, math Div]
          , [ math Add, math Sub]
          , [ inf Cons AssocRight, inf Cat AssocRight]
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
        <|> try parseLambda
        <|> parseSnippet
        <|> parseList
        <|> parseNum
        <|> parseIf
        <|> parseCase
        <|> try parsePitchClass
        <|> try parseApp
        <|> parseVar
        <|> parseBool

parseNote :: Parser Expr
parseNote = b_parens (Note <$> parseExpr <*> parseExpr <*> parseExpr)

parseRest :: Parser Expr
parseRest = Rest <$> b_parens (b_reserved "rest" *> parseExpr)

parseLambda :: Parser Expr
parseLambda = b_parens (Lambda <$> args <*> body)
    where
        args = b_resop "\\" *> manyTill parsePat (b_resop "->")
        body = try bodyStmt <|> bodyExpr
        bodyExpr = Return <$> parseExpr <* option "" b_semi -- sugar for e.g. (\ a b -> a + b)
        bodyStmt = do
            bod <- manyTill (parseAssign <* option "" b_semi) (try $ lookAhead parseReturn)
            ret <- parseReturn <* option "" b_semi
            return (Seq (bod ++ [ret]))

parseSnippet :: Parser Expr
parseSnippet = Snippet <$> b_braces (b_commaSep parseExpr)

parseVar :: Parser Expr
parseVar = Var <$> b_identifier

parsePitchClass :: Parser Expr
parsePitchClass = PitchClass <$> (read <$> parser) <*> getLoc
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

-- So here's the deal: Many languages have a unary + operator. It doesn't
-- actually do anything beyond type promotion. (e.g. +x if x is an unsigned short
-- yields x as a signed integer) In other languages it's a nop.
-- Since "-" (unary minus) is handled as an operation, I'm simplifying this
-- funcion to just parse numbers.
-- (There was a bug here: "+5" would return -5.)
parseNum :: Parser Expr
parseNum = either N D <$> b_number <*> getLoc

-- TODO this is terribly hacky, but it's the easiest way to make sure x (y + z)
-- is treated as 2 expressions, not as a single function application! (note the
-- whitespace!!)
parseApp :: Parser Expr
parseApp = App <$> (ident <* notFollowedBy b_whitespace) <*> b_parens (b_commaSep parseExpr)
    where ident = (:) <$> (lower <|> char '_') <*> many (alphaNum <|> char '_' <|> char '-')

parseIf :: Parser Expr
parseIf = If <$> (b_reserved "if" *> parseExpr)
             <*> (b_reserved "then" *> parseExpr)
             <*> (b_reserved "else" *> parseExpr)

parseCase :: Parser Expr
parseCase = Case <$> (b_reserved "case" *> parseExpr <* b_reserved "of")
                 <*> b_semiSep1 parsePats
    where parsePats = (,) <$> (parsePat <* b_resop "->") <*> parseExpr

-- ===================
-- Parsing Patterns
-- ===================

parsePat :: Parser Pat
parsePat = try parsePatPC
       <|> try parsePatNum
       <|> try parsePatBool
       <|> try parsePatNote
       <|> try parsePatRest
       <|> try parsePatSplit
       <|> parsePatList
       <|> parsePatSnippet
       <|> try parsePatWC
       <|> parsePatVar

parsePatPC :: Parser Pat
parsePatPC = Ppc . read <$> choice (map (try . b_symbol) pitchClasses)

parsePatNum :: Parser Pat
parsePatNum = do
    sign <- optionMaybe (oneOf "-+")
    p <- b_number
    return $ case p of
        Left i -> Pn (signed sign i)
        Right d -> Pd (signed sign d)
    where signed s = (*) (maybe 1 (const (-1)) s)

parsePatBool :: Parser Pat
parsePatBool = do
    (B b) <- parseBool
    return (Pb b)

parsePatNote :: Parser Pat
parsePatNote = b_parens (Pnote <$> parsePat <*> parsePat <*> parsePat)

parsePatRest :: Parser Pat
parsePatRest = Prest <$> b_parens (b_reserved "rest" *> parsePat)

parsePatSplit :: Parser Pat
parsePatSplit = b_parens (chainr1 term (b_resop ":" *> return Psplit))
    where term = parsePat

parsePatList :: Parser Pat
parsePatList = Plist <$> b_brackets (b_commaSep parsePat)

parsePatSnippet :: Parser Pat
parsePatSnippet = Psnip <$> b_braces (b_commaSep parsePat)

parsePatWC :: Parser Pat
parsePatWC = Pwc <$ b_reserved "_"

parsePatVar :: Parser Pat
parsePatVar = flip (maybe Pvar (flip Ppat)) <$> b_identifier <*> optionMaybe (b_resop "@" *> parsePat)
-- parsePatVar = do
--     name <- b_identifier
--     at <- optionMaybe (b_resop "@" *> parsePat)
--     return $ case at of
--         Just pat -> Ppat name pat
--         Nothing -> Pvar name

-- ============
-- Utility
-- ============

getLoc :: Parser (Int,Int)
getLoc = do
    pos <- getPosition
    let loc = (sourceLine pos, sourceColumn pos)
    return loc

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
