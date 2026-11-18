module PatternTests

open Expecto

open XParsec
open XParsec.ActiveParsers

let cursorOf (s: string) =
    ActiveParser.ofReader (Reader.ofString s ())

[<Tests>]
let tests =
    testList
        "Patterns"
        [
            test "PChar matches and advances" {
                match cursorOf "ab" with
                | PChar 'a' next -> "" |> Expect.equal next.Index 1
                | _ -> failtest "PChar 'a' should match"
            }

            test "PChar does not match another character or the end" {
                match cursorOf "b" with
                | PChar 'a' _ -> failtest "PChar 'a' should not match 'b'"
                | _ -> ()

                match cursorOf "" with
                | PChar 'a' _ -> failtest "PChar 'a' should not match the end"
                | _ -> ()
            }

            test "Nested patterns parse in sequence" {
                match cursorOf "abc" with
                | PChar 'a' (AnyChar(b, PChar 'c' (EOF & next))) ->
                    "" |> Expect.equal b 'b'
                    "" |> Expect.equal next.Index 3
                | _ -> failtest "Should match abc"
            }

            test "A failed row leaves the input to the next row" {
                let result =
                    match cursorOf "ab!" with
                    | PChar 'a' (PChar 'b' (PChar 'c' next)) -> "abc", next.Index
                    | PChar 'a' (PChar 'b' next) -> "ab", next.Index
                    | _ -> "none", 0

                "" |> Expect.equal result ("ab", 2)
            }

            test "PString matches a whole string only" {
                match cursorOf "letter" with
                | PString "let" next -> "" |> Expect.equal next.Index 3
                | _ -> failtest "PString should match"

                match cursorOf "le" with
                | PString "let" _ -> failtest "PString should not match a prefix"
                | _ -> ()

                match cursorOf "x" with
                | PString "" next -> "" |> Expect.equal next.Index 0
                | _ -> failtest "An empty PString should match"
            }

            test "Until binds the text before the delimiter and skips it" {
                match cursorOf "// note\nrest" with
                | PString "//" (Until "\n" (text, next)) ->
                    "" |> Expect.equal text " note"
                    "" |> Expect.equal next.Index 8
                | _ -> failtest "Until should match"

                match cursorOf "no end" with
                | Until "*/" _ -> failtest "Until should not match without the delimiter"
                | _ -> ()
            }

            test "Delimited binds the text between two delimiters" {
                match cursorOf "\"hello\" world" with
                | Delimited "\"" (text, next) ->
                    "" |> Expect.equal text "hello"
                    "" |> Expect.equal next.Index 7
                | _ -> failtest "Delimited should match"
            }

            test "Spaces always matches, Spaces1 needs one space" {
                match cursorOf "x" with
                | Spaces next -> "" |> Expect.equal next.Index 0
                | _ -> failtest "Spaces should match nothing"

                match cursorOf "x" with
                | Spaces1 _ -> failtest "Spaces1 should not match"
                | _ -> ()

                match cursorOf " \t x" with
                | Spaces1 next -> "" |> Expect.equal next.Index 3
                | _ -> failtest "Spaces1 should match"
            }

            test "Parsed runs an XParsec parser" {
                match cursorOf "42+" with
                | Parsed CharParsers.pint32 (n, PChar '+' next) ->
                    "" |> Expect.equal n 42
                    "" |> Expect.equal next.Index 3
                | _ -> failtest "Parsed should match"
            }

            test "Skipped runs an XParsec parser and drops its value" {
                match cursorOf "abc!" with
                | Skipped (CharParsers.pstring "abc") (PChar '!' next) -> "" |> Expect.equal next.Index 4
                | _ -> failtest "Skipped should match"
            }

            test "Generic patterns work on any item type" {
                let cursor = ActiveParser.ofReader (Reader.ofArray [| 1; 2; 3; 9 |] ())
                let isBig x = x > 5

                match cursor with
                | Item 1 (ItemSeq [| 2; 3 |] (InRange 5 10 (n, AnyItem _))) -> failtestf "No item is left, got %d" n
                | Item 1 (ItemSeq [| 2; 3 |] (Satisfy isBig (n, EOF))) -> "" |> Expect.equal n 9
                | _ -> failtest "Generic patterns should match"
            }

            test "Combinator patterns run active parsers turned into parsers" {
                let a =
                    ActiveParser.toParser (fun cursor ->
                        match cursor with
                        | PChar 'a' next -> ValueSome('a', next)
                        | _ -> ValueNone
                    )

                match cursorOf "aab" with
                | Many a (xs, PChar 'b' next) ->
                    "" |> Expect.sequenceEqual xs [ 'a'; 'a' ]
                    "" |> Expect.equal next.Index 3
                | _ -> failtest "Many should match"

                match cursorOf "b" with
                | Many a (xs, next) ->
                    "" |> Expect.isEmpty xs
                    "" |> Expect.equal next.Index 0
                | _ -> failtest "Many should match nothing"
            }

            test "Patterns roll back the user state of a row that fails" {
                let cursor = ActiveParser.ofReader (Reader.ofString "ab" 0)
                let count = Parsers.updateUserState ((+) 1)

                let state =
                    match cursor with
                    | Skipped count (PChar 'a' (Skipped count (PChar 'x' next))) -> next.State
                    | Skipped count next -> next.State
                    | _ -> -1

                // The first row counted twice before it failed; the second row starts again from 0.
                "" |> Expect.equal state 1
            }
        ]
