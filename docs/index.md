# XParsec.ActiveParsers

XParsec.ActiveParsers lets you write [XParsec](https://roboz0r.github.io/XParsec/) parsers as F# **active patterns** and match them directly in `match` expressions. Each arm of a `match` is an alternative and each nested pattern is the next step in a sequence, so a grammar reads like the data it parses.

```bash
dotnet add package XParsec.ActiveParsers
```

```fsharp
open XParsec
open XParsec.CharParsers
open XParsec.ActiveParsers

type Cursor = Cursor<char, unit, ReadableString>

type Expr =
    | Num of int
    | Add of Expr * Expr

[<return: Struct>]
let rec (|Expr|_|) (cursor: Cursor) =
    match cursor with
    | Term(a, Spaces(PChar '+' (Spaces(Expr(b, next))))) -> ValueSome(Add(a, b), next)
    | Term(a, next) -> ValueSome(a, next)
    | _ -> ValueNone

and [<return: Struct>] (|Term|_|) (cursor: Cursor) =
    match cursor with
    | Parsed pint32 (n, next) -> ValueSome(Num n, next)
    | PChar '(' (Spaces(Expr(e, Spaces(PChar ')' next)))) -> ValueSome(e, next)
    | _ -> ValueNone

let expr = ActiveParser.toParser (|Expr|_|)
// expr (Reader.ofString "1 + (2 + 3)" ())
// -> Ok (Add (Num 1, Add (Num 2, Num 3)))
```

## Cursors

A `Reader` is a mutable cursor, but every arm of a `match` has to start from the same input. Active patterns therefore take a `Cursor`: an immutable view of a reader at a fixed `Position`. A pattern that matches returns its value and the `Cursor` after it. A pattern that does not match leaves its input untouched, so when an arm fails partway, the next arm starts from the beginning again, just as `<|>` always backtracks.

The position includes the user state, so state changes made by an arm that fails are rolled back too.

An active parser is any function of this shape:

```fsharp
type ActiveParser<'Parsed, 'T, 'State, 'Input> =
    Cursor<'T, 'State, 'Input> -> ('Parsed * Cursor<'T, 'State, 'Input>) voption
```

Mark it `[<return: Struct>]` and give it a `(|Name|_|)` name, and it becomes an active pattern.

## Patterns

A pattern that parses a value binds the value and the cursor after it, `Name (value, next)`. A pattern that only consumes input binds the cursor after it, `Name next`. Nest the next pattern in place of `next` to parse what follows.

Every parser of `Parsers`, `CharParsers` and `ByteParsers`, and every combinator that a `match` does not already express, has a pattern, named after it, that runs it: the pattern matches exactly what the parser accepts. Some variants need no pattern of their own:

- `...Return` (`charReturn`, `stringReturn`, `itemReturn`, `newlineReturn`, ...): return the value from the arm, `| PString "true" next -> ValueSome(true, next)`.
- `...L` (`satisfyL`, `anyStringL`, ...): patterns carry no error; label the parser built with `ActiveParser.toParserL`.
- `skip...` (`skipChar`, `skipAnyOf`, `skipAnyInRange`, ...): ignore the value, `| AnyOf "+-" (_, next)`; the patterns of parsers that return a known value (`pchar`, `pitem`, `pstring`) already bind only `next`.
- `preturn`, `pzero`, `fail`: an arm that returns `ValueSome`, and the arm `| _ -> ValueNone`.
- `getPosition`, `setPosition`: the cursor is the position; `ActiveParser.seek` moves a reader to it.
- `fold`, `foldUserState`: fold the values bound by `Many`.
- Sequencing (`>>.`, `.>>`, `.>>.`, `tuple2`…`tuple5`, `pipe2`…`pipe5`, `bind`): nest the patterns. Mapping (`|>>`, `>>%`): compute the value in the arm. Alternatives (`<|>`, `<|>%`): write the arms. `dispatch`: match the next item.

### Running other parsers

| Pattern | Binds | Matches |
|---|---|---|
| `Parsed p` | `(value, next)` | when the XParsec parser `p` succeeds |
| `Skipped p` | `next` | when `p` succeeds, dropping its value |
| `Active p` | `(value, next)` | when the active parser `p` matches |

### Parsers

| Pattern | Parser | Binds |
|---|---|---|
| `EOF` | `eof` | nothing |
| `AnyItem` | `pid`, `skip` | `(item, next)` |
| `Item x` | `pitem`, `skipItem` | `next` |
| `Satisfy f` | `satisfy` | `(item, next)` |
| `AnyOf xs` / `NoneOf xs` | `anyOf` / `noneOf` | `(item, next)` |
| `InRange a b` | `anyInRange` | `(item, next)` |
| `ItemSeq xs` | `pseq` | `next` |
| `UserState s` | `getUserState` | the state; always matches |
| `UserStateSatisfies f` | `userStateSatisfies` | nothing |
| `SetUserState s` / `UpdateUserState f` | `setUserState` / `updateUserState` | `next`, with the new state |

### Char parsers

| Pattern | Parser | Binds |
|---|---|---|
| `PChar c` | `pchar`, `skipChar` | `next` |
| `AnyChar` | `anyChar` | `(char, next)` |
| `PString s` / `PStringCI s` | `pstring` / `stringCIReturn` | `next` |
| `AsciiLetter` / `Digit` | `asciiLetter` / `digit` | `(char, next)` |
| `Newline` | `newline` | `next` |
| `Spaces` / `Spaces1` | `spaces` / `spaces1` | `next` |
| `ManyChars p` / `Many1Chars p` | `manyChars` / `many1Chars` | `(text, next)` |
| `Many1Chars2 first p` | `many1Chars2` | `(text, next)` |
| `ManyCharsTill p pEnd` | `manyCharsTill` | `((text, endValue), next)` |
| `PInt8` … `PInt64`, `PUInt8` … `PUInt64` | `pint8` … `puint64` | `(number, next)` |
| `PBigInt` / `PFloat` | `pbigint` / `pfloat` | `(number, next)` |
| `AnyString xs` / `AnyStringCI xs` / `AnyStringBy comparison xs` | `anyString` / `anyStringCI` / `anyStringBy` | `(string, next)` |
| `AnyStringReturn pairs` / `AnyStringCIReturn pairs` / `AnyStringByReturn comparison pairs` | `anyStringReturn` / … | `(value, next)` |
| `Until s` | — | `(text, next)`: the text up to the delimiter `s`, then `s` |
| `Delimited s` | — | `(text, next)`: `s`, the text up to the next `s`, then `s` |

A pattern's arguments are evaluated each time it is tried. `AnyString...` builds its lookup of strings each time; in a hot loop, build the parser once and match it with `Parsed`.

### Combinators

The combinator patterns take XParsec parsers, as their combinators do. To use an active parser, turn it into a parser with `ActiveParser.toParser`.

| Pattern | Combinator | Binds |
|---|---|---|
| `Between pOpen pClose p` | `between` | `(value, next)` |
| `Choice ps` | `choice` | `(value, next)` |
| `Opt p` | `opt` | `(value voption, next)`; always matches |
| `Optional p` | `optional` | `next`; always matches |
| `NotEmpty p` | `notEmpty` | `(value, next)` |
| `FollowedBy p` / `NotFollowedBy p` | `followedBy` / `notFollowedBy` | nothing; consumes nothing |
| `LookAhead p` | `lookAhead` | `value` only; consumes nothing |
| `PArray n p` | `parray` | `(values, next)` |
| `Many p` / `Many1 p` | `many` / `many1` | `(values, next)` |
| `Many1Items2 p1 p` | `many1Items2` | `(values, next)` |
| `CountManySatisfies f` / `CountMany1Satisfies f` | `countManySatisfies` / `countMany1Satisfies` | `(count, next)` |
| `SepBy p sep` / `SepBy1 p sep` | `sepBy` / `sepBy1` | `(struct (values, separators), next)` |
| `SepEndBy p sep` / `SepEndBy1 p sep` | `sepEndBy` / `sepEndBy1` | `(struct (values, separators), next)` |
| `ManyTill p pEnd` / `Many1Till p pEnd` | `manyTill` / `many1Till` | `(struct (values, endValue), next)` |
| `Chainl1 p op` / `Chainr1 p op` | `chainl1` / `chainr1` | `(value, next)` |
| `Chainl p op orElse` / `Chainr p op orElse` | `chainl` / `chainr` | `(value, next)`; always matches |

The values of `Many`, `SepBy` and the others are `ImmutableArray`s, as from their combinators.

### Byte parsers

`PFloat16BE`, `PFloat32LE`, `PUInt16BE`, `PInt64LE`, … : one pattern for each parser of `ByteParsers`, with the same name, binding `(number, next)`:

```fsharp
match cursor with
| PUInt16BE(tag, PInt32LE(length, next)) -> ...
```

Like `ByteParsers`, they are not available with Fable, and the floating-point, 128-bit and native-sized integer patterns need .NET 8 or later.

Pattern arguments must be identifiers, literals or parenthesized function applications such as `(pchar ',')`. Lambdas and operators are not allowed, so bind them to a name first: `let isDigit c = Char.IsDigit c`, then `Satisfy isDigit (c, next)`; `let minus = pchar '-' >>% (-)`, then `Chainl1 pint32 minus (n, next)`.

## From and to XParsec parsers

- `ActiveParser.ofParser p` turns an XParsec parser into an active parser. The `Parsed` pattern uses it.
- `ActiveParser.toParser p` turns an active parser into an XParsec parser. When nothing matches, it fails with `ActiveParser.noMatch` and leaves the reader where it started. `ActiveParser.toParserL message p` fails with `message` instead.
- `ActiveParser.map f p` maps the value of an active parser.

Because both sides convert freely, you can use active patterns for the parts of a grammar where they read best and combinators everywhere else, for example `sepBy (ActiveParser.toParser (|Expr|_|)) (pchar ';')`.

## Active patterns in the parser computation expression

`ActiveParser.cursor` returns the current cursor, and `ActiveParser.seek next` moves the reader to a cursor. Together they let `match!` match active patterns inside `parser { }`:

```fsharp
let identifier = many1Chars (satisfy System.Char.IsLetter)

let pLet =
    parser {
        match! ActiveParser.cursor with
        | PString "let" (Spaces1(Parsed identifier (name, next))) ->
            do! ActiveParser.seek next
            do! spaces
            do! skipChar '='
            do! spaces
            let! value = expr
            return name, value
        | _ -> return! fail (Message "Expected let")
    }
```

## Tokenizers with `unfold`

`ActiveParser.unfold step` runs `step` from a cursor, then from the cursor it returns, and so on. Each step returns an `Unfold` value that says whether to go on (`Continue`, or `ContinueWith value` to add a value) or to stop (`Break`, or `BreakWith value`). `unfold` returns every value it collected, and always matches.

```fsharp
type Token =
    | Ident of string
    | Number of int
    | Symbol of char
    | Comment of string

let tokens =
    ActiveParser.unfold (fun (cursor: Cursor) ->
        match cursor with
        | EOF -> Break, cursor
        | Spaces1 next -> Continue, next
        | PString "//" (Until "\n" (text, next)) -> ContinueWith(Comment text), next
        | Parsed identifier (name, next) -> ContinueWith(Ident name), next
        | Parsed pint32 (n, next) -> ContinueWith(Number n), next
        | AnyChar(c, next) -> ContinueWith(Symbol c), next
        | _ -> Break, cursor)

// ActiveParser.toParser tokens (Reader.ofString "let x = 42 // answer\n;" ())
// -> Ok [ Ident "let"; Ident "x"; Symbol '='; Number 42; Comment " answer"; Symbol ';' ]
```

Like `many`, `unfold` raises `InfiniteLoopException` when a `Continue` or `ContinueWith` step returns its cursor unchanged.

## Errors

A pattern that does not match only says "no". The error of a failed XParsec parser inside `Parsed` is dropped. To report a useful error, label the converted parser with `ActiveParser.toParserL "expression"` or `<?>`, or end the `match` with an arm that fails with a message of your own.
