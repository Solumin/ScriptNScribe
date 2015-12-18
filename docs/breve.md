# Breve Language Specification

## Grammar

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

unop ::= '!' | '-'

binop ::= mathop | eqop | ordop | catop
mathop ::= '+' | '-' | '\*' | '/'
eqop ::= "==" | "!="
ordop ::= "<=" | '<' | '>' | ">="
catop ::= ':' | "++" | ":+:" | ":=:"

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

assign ::= identifier '=' expr
return ::= "return" expr
sequence ::= statement {';' statement}
statement ::= assign | return | sequence

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

lineComment ::= --
blockComment ::= {- ... -}
```
