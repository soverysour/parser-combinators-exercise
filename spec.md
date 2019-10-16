### Specification

#### Alphabet

- Upper and lowecase 'a' - 'z' letters of the English alphabet.
- Digits (0 - 9)
- Underline character '_'
- Punctuation symbols ( ) ; : " = + - * /

#### Lexical rules

```lexer
identifier = <ident-start> {<ident-body>}
ident-start = "a" | ... | "z" | "A" | ... | "Z" | "_"
ident-body = <ident-start> | <digit>

int-lit = "0" | [("+"|"-")] <non-zero-digit> {<digit>}
non-zero-digit = "1" | ... | "9"
digit = "0" | <non-zero-digit>

string-lit = \" ^(") \"
punctuation = "{" | "(" | ")" | "}" | ";" | ":" | "+" | "-" | "*" | "/"
```

#### Syntactic rules

```parser
program = <stmt> {<stmt>}

stmt = "let" {<decl>;} <decl> "in" <stmt>
     | "if" <assign-val> "then" <stmt> "else" <stmt>
     | "read" <identifier>
     | "write" <stmt>
     | "loop" <stmt> "while" <assign-val>
     | "do" <stmt> {<stmt>} ";"

decl = <identifier> ":" <assign-type> "=" <assign-val>

assign-val  = <expr> | <compound-assig-val>
assign-type = "String" | "Int" | <compount-type>

expr = <term> "+" <expr>
     | <term> "-" <expr>
     | <term>
     
term = <factor> "*" <expr>
     | <factor> "/" <expr>
     | <factor>
     
factor = "(" <expr> ")"
       | <identifier>
       | <int-lit>
       | <string-lit>
       
compount-type = "(" <assign-type> "," <assign-type> ")"
compound-assign-val = "(" <assign-val> "," <assign-val> ")"
```

```keywords
- let
- in
- if
- then
- else
- read
- write
- loop
- while
- do
- String
- Int
```

```
    Token codes
-------------------
 Identifier    |  0
 constant      |  1
 (             |  2
 )             |  3
 ;             |  4
 :             |  5
 ,             |  6
 =             |  7
 +             |  8
 -             |  9
 *             | 10
 /             | 11
 let           | 12
 in            | 13
 if            | 14
 then          | 15
 else          | 16
 read          | 17
 write         | 18
 loop          | 19
 while         | 20
 do            | 21
 Int           | 22
 String        | 23
```
