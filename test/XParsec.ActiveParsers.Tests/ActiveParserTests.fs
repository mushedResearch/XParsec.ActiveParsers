module ActiveParserTests

open Expecto

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.Combinators
open XParsec.ActiveParsers

type Cursor = Cursor<char, unit, ReadableString>

type Expr =
    | Num of int
    | Add of Expr * Expr
    | Mul of Expr * Expr

// A grammar written with recursive active patterns, in the style of ActiveParsers.
[<return: Struct>]
let rec (|Expr|_|) (cursor: Cursor) =
    match cursor with
    | Term(a, Spaces(PChar '+' (Spaces(Expr(b, next))))) -> ValueSome(Add(a, b), next)
    | Term(a, next) -> ValueSome(a, next)
    | _ -> ValueNone

and [<return: Struct>] (|Term|_|) (cursor: Cursor) =
    match cursor with
    | Factor(a, Spaces(PChar '*' (Spaces(Term(b, next))))) -> ValueSome(Mul(a, b), next)
    | Factor(a, next) -> ValueSome(a, next)
    | _ -> ValueNone

and [<return: Struct>] (|Factor|_|) (cursor: Cursor) =
    match cursor with
    | Parsed pint32 (n, next) -> ValueSome(Num n, next)
    | PChar '(' (Spaces(Expr(e, Spaces(PChar ')' next)))) -> ValueSome(e, next)
    | _ -> ValueNone

type Token =
    | Ident of string
    | Number of int
    | Symbol of char
    | Comment of string

let identifier = many1Chars (satisfy System.Char.IsLetter)

let tokens =
    ActiveParser.unfold (fun (cursor: Cursor) ->
        match cursor with
        | EOF -> Break, cursor
        | Spaces1 next -> Continue, next
        | PString "//" (Until "\n" (text, next)) -> ContinueWith(Comment text), next
        | Parsed identifier (name, next) -> ContinueWith(Ident name), next
        | Parsed pint32 (n, next) -> ContinueWith(Number n), next
        | AnyChar(c, next) -> ContinueWith(Symbol c), next
        | _ -> Break, cursor
    )

let run (p: Parser<'A, char, unit, ReadableString>) s =
    let reader = Reader.ofString s ()
    p reader, reader.Index

[<Tests>]
let tests =
    testList
        "ActiveParser"
        [
            test "toParser runs a grammar of active patterns" {
                let result, index = run (ActiveParser.toParser (|Expr|_|)) "1 + 2 * (3 + 4)"

                "" |> Expect.equal result (Ok(Add(Num 1, Mul(Num 2, Add(Num 3, Num 4)))))
                "" |> Expect.equal index 15
            }

            test "toParser leaves the reader where it started when nothing matches" {
                let result, index = run (ActiveParser.toParser (|Expr|_|)) "(1 *"

                match result with
                | Error e -> "" |> Expect.equal e.Errors ActiveParser.noMatch
                | Ok x -> failtestf "Should fail, got %A" x

                "" |> Expect.equal index 0
            }

            test "toParserL fails with its message" {
                let result, _ = run (ActiveParser.toParserL "expression" (|Expr|_|)) "x"

                match result with
                | Error e -> "" |> Expect.equal e.Errors (Message "expression")
                | Ok x -> failtestf "Should fail, got %A" x
            }

            test "ofParser and toParser compose with XParsec combinators" {
                let expr = ActiveParser.toParser (|Expr|_|)
                let result, _ = run (sepBy expr (pchar ';')) "1;2+3"

                match result with
                | Ok(struct (xs, _)) -> "" |> Expect.sequenceEqual xs [ Num 1; Add(Num 2, Num 3) ]
                | Error e -> failtestf "%A" e
            }

            test "map changes the value of an active parser" {
                let result, _ = run (ActiveParser.toParser (ActiveParser.map string (|Expr|_|))) "7"
                "" |> Expect.equal result (Ok "Num 7")
            }

            test "unfold collects the values of its steps" {
                let result, index = run (ActiveParser.toParser tokens) "let x = 42 // answer\n;"

                match result with
                | Ok xs ->
                    ""
                    |> Expect.sequenceEqual
                        xs
                        [ Ident "let"; Ident "x"; Symbol '='; Number 42; Comment " answer"; Symbol ';' ]
                | Error e -> failtestf "%A" e

                "" |> Expect.equal index 22
            }

            test "unfold stops at BreakWith" {
                let upToSemicolon =
                    ActiveParser.unfold (fun (cursor: Cursor) ->
                        match cursor with
                        | PChar ';' next -> BreakWith ';', next
                        | AnyChar(c, next) -> ContinueWith c, next
                        | _ -> Break, cursor
                    )

                let result, index = run (ActiveParser.toParser upToSemicolon) "ab;cd"

                match result with
                | Ok xs -> "" |> Expect.sequenceEqual xs [ 'a'; 'b'; ';' ]
                | Error e -> failtestf "%A" e

                "" |> Expect.equal index 3
            }

            test "unfold raises InfiniteLoopException when a step does not advance" {
                let stuck = ActiveParser.unfold (fun (cursor: Cursor) -> Continue, cursor)

                Expect.throwsT<InfiniteLoopException<unit>>
                    (fun () -> run (ActiveParser.toParser stuck) "a" |> ignore)
                    ""
            }

            test "parser CE matches active patterns with cursor and seek" {
                let pLet =
                    parser {
                        match! ActiveParser.cursor with
                        | PString "let" (Spaces1(Parsed identifier (name, next))) ->
                            do! ActiveParser.seek next
                            do! spaces
                            do! skipChar '='
                            do! spaces
                            let! value = ActiveParser.toParser (|Expr|_|)
                            return name, value
                        | _ -> return! fail (Message "Expected let")
                    }

                let result, index = run pLet "let x = 1 + 2"
                "" |> Expect.equal result (Ok("x", Add(Num 1, Num 2)))
                "" |> Expect.equal index 13

                let result, _ = run pLet "var x = 1"

                match result with
                | Error e -> "" |> Expect.equal e.Errors (Message "Expected let")
                | Ok x -> failtestf "Should fail, got %A" x
            }
        ]
