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
pitchclass ::= ( A | B | C | D | E | F | G ) ( ff | f | ss | s )
integer ::= digit {digit}
double ::= integer '.' integer
boolean ::= "true" | "false"
note = '(' expr expr expr ')'
rest ::= '(' "rest" expr ')'
snippet ::= '{' expr {',' expr } '}'
identifier ::= (lowercase | _ ) {letter | digit | underscore}
var ::= identifier
list = '[' expr {',' expr} ']'

expr ::= pitchclass
       | integer | double | boolean
       | note | rest
       | snippet | list
       | expr binop expr
       | unop expr
       | identifier
       | '(\' ident {ident} '->' (expr | assign {assign} return ')'
       | identifier'(' {expr} ')'
       | 'if' expr 'then' expr 'else' expr
       | 'case' expr 'of' {pattern '->' expr ';'}
       | '(' expr ')'

assign ::= identifier '=' expr
return ::= "return" expr
sequence ::= statement {';' statement}
statement ::= assign | return | sequence

pattern ::= pitchclass
          | integer
          | double
          | boolean
          | '(' pattern pattern pattern ')'
          | '(' 'rest' pattern ')'
          | '[' pat {',' pat} ']'
          | '[' ']'
          | '(' pat ':' pat {':' pat} ')'
          | '{' pat {',' pat} '}'
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

-- Loc is a the (line, column) location of a literal, used as part of traces
-- during synthesis. Note these do not necessarily map 100% accurately to the
-- location of the literal; it is only necessary that each literal has a unique
-- location such that loc(A) < loc(B) if A appears before B in the source.
-- (The second property is useful, but not absolutely 100% necessary.)
type Loc = (Int, Int)

-- Expr represents an expression in Breve.
-- See langauge definition above.
data Expr = PitchClass E.PitchClass Loc
          | N Integer Loc | D Double Loc
          | B Bool
          | UnOpExpr UnOp Expr
          | BinOpExpr BinOp Expr Expr
          | Note Expr Expr Expr -- PitchClass, Octave, Duration
          | Rest Expr           -- Duration
          | Snippet [Expr]      -- Note | Rest
          | Var String
          | List [Expr]
          | Lambda [Pat] Statement  -- Seq or Return, most likely
          | App String [Expr]       -- Function name, arguments
          | If Expr Expr Expr       -- If statement: condition, true branch, false branch
          | Case Expr [(Pat, Expr)] -- case expr of pat -> expr; ...
          deriving (Eq)

-- Specialized Show (toString) for Expr.
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

-- Binary Operators, such as addition, multiplication, etc.
data BinOp =
      SeqOp | ParOp                     -- snippets
    | Add | Mult | Div | Sub            -- math
    | Eq | Neq | Lt | Lte | Gt | Gte    -- equality
    | Cons | Cat                        -- list
    deriving (Eq)

-- The string representations of BinOps are used during parsing. See the optable
-- below.
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

-- Unary operators. Right now just negation for booleans and numbers.
-- You may have noticed the language specification does not include negative
-- integers. This is due to the negation operator and how Parsec handles numbers
-- and trust me it's just EASIER this way.
data UnOp = Not | Neg deriving (Eq)
instance Show UnOp where
    show Not = "!"
    show Neg = "-"

-- Statement represents a full line of a breve program: an assignment, a return
-- (only valid inside lambdas) and a sequence of statements.
-- Statements end with semicolons.
data Statement = Assign String Expr | Return Expr | Seq [Statement] deriving (Eq)

-- Interestingly, the show instances we have so far are basically sufficient for
-- recreating the source of a program simply by calling "show".
-- However, comments are lost -- again due to parsec.
instance Show Statement where
    show (Assign s e) = unwords [s, "=", shows e ";"]
    show (Seq ss) = unlines (map show ss)
    show (Return e) = "return " ++ shows e ";"

-- Pat represents a pattern matching construct, used in case expressions and
-- lambdas.
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
    | Pwc               -- wildcard
    | Ppat String Pat   -- at pattern, e.g. l@(x:xs)
      -- Splitting
    | Psplit Pat Pat    -- (:) for splitting list and snippet elements off
    deriving (Show, Eq)

-- All the pitches possible in the language. The order of the m list is
-- significant.
pitchClasses = [n : m | n <- ['A'..'G'], m <- ["ff", "ss", "f", "s", ""]]
-- Current language keywords. Some of them are "reserved" for future use
-- (namely def)
keywords = ["rest", "true", "false", "if", "then", "else", "def", "return", "case", "of"]

-- Operators. Neat how we can have one canonical representation for each op by
-- implementing Show, huh?
mathOps = map show [Add, Sub, Mult, Div]
unOps = map show [Neg, Not]
boolOps = map show [Eq, Neq, Lt, Lte, Gt, Gte]
listOps = map show [Cons, Cat]
catOps = map show [SeqOp, ParOp] ++ ["=", "\\", "->", "@"]

-- The language definition used by Parsec to generate parsers.
-- This is somewhat more broad than it needs to be, but that allows for
-- expansion of the language.
-- (For example, opStart and opLetter are primarily for user-defined ops, which
-- are not going to be a thing in Breve any time soon.)
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
                    , reservedOpNames = catOps ++ mathOps ++ unOps ++ boolOps ++ listOps
                    , caseSensitive = True
                    }

-- This bizarre-looking reversed construct is used to extract the language
-- parsers we need from the call to "makeTokenParser".
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

-- The main hook for calling the breveParser.
-- Should be with "runParser".
breveParser :: Parser Statement
breveParser = b_whitespace >> parseSeq <* eof

-- ===================
-- Parsing Statements
-- ===================

-- All of the parsers are named very clearly:
-- parse<Expr> or parsePat<Pattern>
-- Comments are mainly for explaining rational, since what the thing is
-- attempting to parse should be clear.
-- "do" notation is avoided as much as possible.

-- Parses a sequence of semicolon-separated statements.
parseSeq :: Parser Statement
parseSeq = fmap Seq (b_semiSep1 parseStatement)

-- Parses the other two kinds of statements; return and assign.
-- Return is furst so it can try to eat "return" keywords before parseAssign has
-- a go.
parseStatement :: Parser Statement
parseStatement = parseReturn <|> try parseAssign

parseAssign :: Parser Statement
parseAssign = Assign <$> (b_identifier <* b_resop "=") <*> parseExpr

parseReturn :: Parser Statement
parseReturn = Return <$> (b_reserved "return" *> parseExpr)

-- ===================
-- Parsing Expressions
-- ===================

-- parseExpr was a hard-fought battle of not quite understanding what Parsec was
-- trying to do with parseTerm. However, this makes much more sense now.
-- It's essentially going to use the chainr combinator, which tries to parse a
-- "chain" of an operator. e.g. 1 + 2 + 3 + 4 is implemented using chainr
-- (numberparser) (additionopparser). Given that understanding, we have to
-- define the term parser as either a single term or an (expression in
-- parenthesis)
parseExpr :: Parser Expr
parseExpr = buildExpressionParser opTable term <?> msg
    where
        term = parseTerm <|> b_parens parseExpr <?> msg
        msg = "an expression or operation (the statement ended early!)"

-- Precendence is from top to bottom.
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

-- These are mainly grouped to try to avoid conflicts and overlapping "try"s.
-- Note, Rest, Lambda all start with '('. The next few are all distinct when
-- they start, but then PitchClass (very restricted) overlaps with App (somewhat
-- restricted -- must be ident({expr})) overlaps with Var.
-- Bool is last for no particular reason?
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

-- We try to do something useful here:
-- Lambdas are defined as a sequence of patterns followed by a body.
-- The body is either a single expression or a sequence of assign statements
-- terminated by a return statement. So our parser reflects that: it tries to
-- parse several Assigns, stopping if it encounters a Return; otherwise it tries
-- to parse an expression.
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
-- funcion to just parse numbers. Unary "+" is no longer a thing.
-- (There was a bug here: "+5" would return -5 due to stupidity of handling sign
-- parsing.)
parseNum :: Parser Expr
parseNum = either N D <$> b_number <*> getLoc

-- TODO this is terribly hacky, but it's the easiest way to make sure x (y + z)
-- is treated as 2 expressions, not as a single function application! (note the
-- "notFollowedBy b_whitespace" parser after ident!)
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

-- Since we don't have a negation operator for patterns, we do have to actually
-- try to parse negative numbers.
-- But since we don't have a negation operator for patterns, this is easy.
-- TODO: No do notation?
parsePatNum :: Parser Pat
parsePatNum = do
    sign <- optionMaybe (char '-')
    p <- b_number
    return $ case p of
        Left i -> Pn (signed sign i)
        Right d -> Pd (signed sign d)
    where signed s = (*) (maybe 1 (const (-1)) s)

-- Why waste a perfectly good parseBool?
parsePatBool :: Parser Pat
parsePatBool = fmap (\(B b) -> Pb b) parseBool

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

-- My obsession with removing do notation lead to this particular monstronsity.
-- My Frankenstein's Monster! If we have an identifier followed by an "@", we
-- have an at-pattern and have to assign the rest of the pattern to the
-- identifier before parsing the rest of it.
-- Otherwise it's just a normal pattern variable.
parsePatVar :: Parser Pat
parsePatVar = flip (maybe Pvar (flip Ppat)) <$> b_identifier <*> optionMaybe (b_resop "@" *> parsePat)
-- Do-notation version left for posterity.
-- parsePatVar = do
--     name <- b_identifier
--     at <- optionMaybe (b_resop "@" *> parsePat)
--     return $ case at of
--         Just pat -> Ppat name pat
--         Nothing -> Pvar name

-- ============
-- Utility
-- ============

-- Used by the literal parsers to grab the location from source code.
-- This must be used consistently; if you call it after parsing in one parser,
-- you must do the same everywhere else!
-- This COULD be replaced with Control.Arrow.&&& but I'm not sure if we want
-- that kind of magic here. (Maybe FIXME later)
getLoc :: Parser (Int,Int)
getLoc = do
    pos <- getPosition
    let loc = (sourceLine pos, sourceColumn pos)
    return loc

-- Turn the common durations into their equivalent Euterpea names.
-- This isn't actually used here. Or anywhere.
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
