# XParsec.ActiveParsers

[![Documentation](https://img.shields.io/badge/see_the-docs-blue?style=flat)][DocsLink]
[![NuGet Version](https://img.shields.io/nuget/v/XParsec.ActiveParsers)][NugetLink]

XParsec.ActiveParsers lets you write [XParsec](https://github.com/roboz0r/XParsec) parsers as F# **active patterns** and match them directly in `match` expressions. Each arm of a `match` is an alternative and each nested pattern is the next step in a sequence, so a grammar reads like the data it parses. It ports the techniques of [ActiveParsers](https://github.com/mushedResearch/ActiveParsers) to XParsec, and works on .NET and with [Fable](https://fable.io/).

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

- Every parser of XParsec's `Parsers`, `CharParsers` and `ByteParsers`, and every combinator a `match` does not already express, has a pattern named after it.
- Failed arms backtrack: patterns match an immutable `Cursor`, so every arm starts from the same input, as `<|>` does.
- Active parsers and XParsec parsers convert freely both ways, so you can use patterns where they read best and combinators everywhere else.

See the [documentation][DocsLink] for every pattern, `match!` in `parser { }` and tokenizers with `ActiveParser.unfold`.

## Installation

```bash
dotnet add package XParsec.ActiveParsers
```

## Running Tests

```pwsh
dotnet test
```

[NugetLink]: https://www.nuget.org/packages/XParsec.ActiveParsers
[DocsLink]: https://mushedresearch.github.io/XParsec.ActiveParsers/
