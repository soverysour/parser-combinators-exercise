### Specification

#### Alphabet

- Upper and lowecase 'a' - 'z' letters of the English alphabet.
- Digits (0 - 9)
- Underline character '_'
- Punctuation symbols ( ) ; : " =

#### Lexical rules

```lexer
identifier = <ident-start><ident-body>*
ident-start = "a" | ... | "z" | "A" | ... | "Z" | "_"
ident-body = <ident-start> | <digit>

int-lit = "0" | ("+"|"-")? <non-zero-digit> {<digit>}
non-zero-digit = "1" | ... | "9"
digit = "0" | <non-zero-digit>

string-lit = \" ^(") \"
punctuation = "{" | "(" | ")" | "}" | ";" | ":"
```

#### Syntactic rules

```parser
program = <stmt> {<stmt>}

stmt = "let" {<decl>;} <decl> "in" <stmt>
     | "if" <assign-val> "then" <stmt> "else" <stmt>
     | "read" <identifier>
     | "write" <stmt>
     | "loop" <stmt> "while" <assign-val>
     | "do" <stmt> {<stmt>}

decl = <identifier> ":" <ident-type> "=" <assign-val>

assign-val  = <literal> | <identifier> | <compound-assig-val>
assign-type = "String" | "Int" | <compount-type>

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
```

```
    Token codes
-------------------
 Identifier    |  0
 LitInt        |  1
 LitStr        |  2
 ParenOpen     |  3
 ParenClose    |  4
 Semicolon     |  5
 Colon         |  6
 Comma         |  7
 Equal         |  8
 KwLet         |  9
 KwIn          | 10
 KwIf          | 11
 KwThen        | 12
 KwElse        | 13
 KwRead        | 14
 KwWrite       | 15
 KwLoop        | 16
 KwWhile       | 17
 KwDo          | 18
 KwIntT        | 19
 KwStrT        | 20
```
