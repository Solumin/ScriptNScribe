# Breve Language Specification

## Values

```
pitchclass ::= ( 'A' | 'B' | 'C' | 'D' | 'E' | 'F' | 'G' ) ( "ff" | 'f' | "ss" | 's' )
integer ::= digit {digit}
double ::= integer '.' integer
boolean ::= "true" | "false"
note = '(' expr expr expr ')'
rest ::= '(' "rest" expr ')'
snippet ::= '{' expr {',' expr } '}'
identifier ::= (lowercase | _ ) {letter | digit | underscore}
var ::= identifier
list ::= '[' expr {',' expr} ']'
```

The basic values of Breve. There are some types that matter:

* Notes take a pitch class, integer (octave) and number (duration)
* Rests take a number (duration)
* Lists *should* be homogeneous, but this is not checked in the current version
  of Breve.
* Snippets should have only notes and rests. This *is* checked in the current
  version of Breve.

Breve does not support strings.

## Expressions

```
unop ::= '!' | '-'

binop ::= mathop | eqop | ordop | catop
mathop ::= '+' | '-' | '\*' | '/'
eqop ::= "==" | "!="
ordop ::= "<=" | '<' | '>' | ">="
catop ::= ':' | "++" | ":+:" | ":=:"
```

The operators. Only two unary operators are currently needed: Not (!) and
negate (-). Binary operators are split into basic math, equality, ordering and
concatenation. ":" adds a single element to a list, while "++" combines two
lists. ":+:" adds an element to a snippet, combines two elements into a snippet,
or combines two snippets. ":=:" is similar, but adds the elements in parallel.

```
expr ::= pitchclass
       | integer | double | boolean
       | note | rest
       | snippet | list
       | expr binop expr
       | unop expr
       | identifier
       | '(\' pattern {pattern} '->' (expr | assign {assign} return ')'
       | identifier'(' {expr} ')'
       | 'if' expr 'then' expr 'else' expr
       | 'case' expr 'of' {pattern '->' expr ';'}
       | '(' expr ')'
```

In addition to the value expressions from earlier, Breve has:

* Lambdas: (\ x -> ...). "..." may be a single expression (to allow for e.g `( a
  b -> a + b)`), or a sequence of one or more statements ending with a return
  statement. Note that parameters are patterns, not just names!
* Function application: Not in Haskell style, unlike other parts of the
  language. Simply `foo(a,b,c)`.
* If expressions: Like Haskell, if statements are instead expressions. Note
  that no parenthesis are required around the conditional.
* Case statements: Like Haskell: case x of ..., except the patterns explicitly
  end with semicolons.

## Statements

```
assign ::= identifier '=' expr
return ::= "return" expr
sequence ::= statement {';' statement}
statement ::= assign | return | sequence
```

Breve programs are sequences of semicolon-termiated statements. The only notable
thing here is that return statements are **only** valid at the end of lambdas.

## Patterns 

```
pattern ::= pitchclass
          | integer
          | double
          | boolean
          | '(' pattern pattern pattern ')'
          | '(' "rest" pattern ')'
          | '[' pat {',' pat} ']'
          | '[' ']'
          | '(' pat ':' pat {':' pat} ')'
          | '{' pat {',' pat} '}'
          | '(' pat ':' '{' '}' ')'
          | identifier
          | '_'
```

Breve's patterns are heavily inspired by Haskell's. They can match pitch
classes, integers and doubles, booleans, notes, rests, lists (including the
empty list), snippets, and variables. The wildcard "\_" has the same meaning as
it does in Haskell, as does the split operator, ":".

One quirk: The empty snippet is allowed *only* as part of a split pattern:
`(x:{})` is legal, but `{}` is not. This is due to the uncertain semantic nature
of the empty snippet, which is explained elsewhere.

## Comments

```
lineComment ::= --
blockComment ::= {- ... -}
```

Breve has comments but the parser doesn't support them very well, and they might
not survive synthesis.
