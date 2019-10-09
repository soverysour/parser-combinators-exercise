```lexer
identifier = <ident-start><ident-body>*
ident-start = a | ... | z | A | ... | Z | _
ident-body = <ident-start> | <digit>

int-lit = 0 | (+|-)? <non-zero-digit> <digit>*
non-zero-digit = 1 | ... | 9
digit = 0 | <non-zero-digit>

string-lit = " ^(") "
punctuation = { | \( | \) | } | ; | :
```

```parser
program = <stmt>+

stmt = let (<decl>;)* <decl> in <stmt>
     | if <assign-val> then <stmt> else <stmt>
     | read <identifier>
     | write <stmt>
     | loop <stmt> while <assign-val>
     | do <stmt>+

decl = <identifier> : <ident-type> \= <assign-val>

assign-val  = <literal> | <identifier> | <compound-assig-val>
assign-type = String | Int | <compount-type>

compount-type = \( <assign-type>, <assign-type> \)
compound-assign-val = \( <assign-val>, <assign-val> \)
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
 BracketOpen   |  3
 BracketClose  |  4
 ParenOpen     |  5
 ParenClose    |  6
 Semicolon     |  7
 Colon         |  8
 Comma         |  9
 Equal         | 10
 KwLet         | 11
 KwIn          | 12
 KwIf          | 13
 KwThen        | 14
 KwElse        | 15
 KwRead        | 16
 KwWrite       | 17
 KwLoop        | 18
 KwWhile       | 19
 KwDo          | 20
 KwIntT        | 21
 KwStrT        | 22
```
