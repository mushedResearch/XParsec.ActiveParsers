module ParityTests

open System

open Expecto

open XParsec
open XParsec.Parsers
open XParsec.CharParsers
open XParsec.ByteParsers
open XParsec.ActiveParsers

/// Runs the pattern and the parser on each input: both must fail, or bind the same value, compared
/// through <c>project</c>, and leave the same index and user state.
let expectParityBy
    (project: 'A -> 'B)
    (name: string)
    (pattern: Cursor<'T, 'State, 'Input> -> ('A * Cursor<'T, 'State, 'Input>) voption)
    (parser: Parser<'A, 'T, 'State, 'Input>)
    (readers: (unit -> Reader<'T, 'State, 'Input>) list)
    =
    for makeReader in readers do
        let patternResult =
            match pattern (ActiveParser.ofReader (makeReader ())) with
            | ValueSome(x, next) -> Some(project x, next.Index, next.State)
            | ValueNone -> None

        let reader = makeReader ()

        let parserResult =
            match parser reader with
            | Ok x -> Some(project x, reader.Index, reader.State)
            | Error _ -> None

        // ER equality, so that a NaN from both sides counts as the same value.
        Expect.isTrue
            (LanguagePrimitives.GenericEqualityER patternResult parserResult)
            $"{name} on input {reader.Input}: the pattern gave %A{patternResult}, the parser %A{parserResult}"

/// Runs the pattern and the parser on each input and compares their values as they are.
let expectParity name pattern parser readers =
    expectParityBy id name pattern parser readers

/// Values of ImmutableArray compare by reference: compare their items.
let items (xs: Collections.Immutable.ImmutableArray<'A>) = List.ofSeq xs

/// A pattern that binds only the cursor, as a pattern that binds unit.
let bindsUnit (pattern: Cursor<'T, 'State, 'Input> -> Cursor<'T, 'State, 'Input> voption) cursor =
    match pattern cursor with
    | ValueSome next -> ValueSome((), next)
    | ValueNone -> ValueNone

let strings =
    [
        ""
        "a"
        "A"
        "1"
        "12"
        "-12"
        "+7"
        "0x1F"
        "0b101"
        "255"
        "256"
        "-129"
        "40000"
        "99999999999999999999"
        "1.5e3"
        "-.5"
        "nan"
        "Infinity"
        "\n"
        "\r\n"
        "\rx"
        " \t x"
        "abc"
        "ABC"
        "aBc"
        "ab1c"
        "let"
        "letter"
        "LET x"
        "x"
        "1,2,3"
        "1,2,"
        "a,b"
        "12;"
        "8-3-2"
        "(5)"
    ]
    |> List.map (fun s () -> Reader.ofString s ())

let integers =
    [ [||]; [| 1 |]; [| 1; 2 |]; [| 2; 3 |]; [| 9 |] ]
    |> List.map (fun xs () -> Reader.ofArray xs ())

let bytes =
    [ 0..17 ]
    |> List.map (fun length () -> Reader.ofArray (Array.init length (fun i -> byte (i * 37 + 1))) ())

[<Tests>]
let tests =
    testList
        "Parity with XParsec parsers"
        [
            test "Parsers" {
                expectParity
                    "EOF"
                    (bindsUnit (fun c ->
                        match c with
                        | EOF -> ValueSome c
                        | _ -> ValueNone
                    ))
                    eof
                    strings

                expectParity "AnyItem" (|AnyItem|_|) pid strings
                expectParity "AnyItem" (|AnyItem|_|) pid integers
                expectParity "Item" (bindsUnit ((|Item|_|) 'a')) (skipItem 'a') strings
                expectParity "Item" (bindsUnit ((|Item|_|) 1)) (skipItem 1) integers
                expectParity "Satisfy" ((|Satisfy|_|) Char.IsDigit) (satisfy Char.IsDigit) strings
                expectParity "AnyOf" ((|AnyOf|_|) "abc") (Parsers.anyOf "abc") strings
                expectParity "AnyOf" ((|AnyOf|_|) [ 1; 9 ]) (Parsers.anyOf [ 1; 9 ]) integers
                expectParity "NoneOf" ((|NoneOf|_|) "abc") (noneOf "abc") strings
                expectParity "NoneOf" ((|NoneOf|_|) [ 1 ]) (noneOf [ 1 ]) integers
                expectParity "InRange" ((|InRange|_|) 'a' 'c') (anyInRange 'a' 'c') strings
                expectParity "InRange" ((|InRange|_|) 2 9) (anyInRange 2 9) integers
                expectParity "ItemSeq" (bindsUnit ((|ItemSeq|_|) "ab")) (pseq "ab" >>% ()) strings
                expectParity "ItemSeq" (bindsUnit ((|ItemSeq|_|) "")) (pseq "" >>% ()) strings
                expectParity "ItemSeq" (bindsUnit ((|ItemSeq|_|) [| 1; 2 |])) (pseq [| 1; 2 |] >>% ()) integers
            }

            test "User state" {
                let readers = [ 0; 1; 5 ] |> List.map (fun state () -> Reader.ofString "x" state)

                expectParity
                    "UserState"
                    (fun c ->
                        match c with
                        | UserState s -> ValueSome(s, c)
                    )
                    getUserState
                    readers

                expectParity
                    "UserStateSatisfies"
                    (bindsUnit (fun c ->
                        match c with
                        | UserStateSatisfies((<) 0) -> ValueSome c
                        | _ -> ValueNone
                    ))
                    (userStateSatisfies ((<) 0))
                    readers

                expectParity "SetUserState" (bindsUnit ((|SetUserState|_|) 7)) (setUserState 7) readers

                expectParity
                    "UpdateUserState"
                    (bindsUnit ((|UpdateUserState|_|) ((+) 1)))
                    (updateUserState ((+) 1))
                    readers
            }

            test "CharParsers: characters and strings" {
                expectParity "PChar" (bindsUnit ((|PChar|_|) 'a')) (skipChar 'a') strings
                expectParity "AnyChar" (|AnyChar|_|) anyChar strings
                expectParity "PString" (bindsUnit ((|PString|_|) "le")) (pstring "le" >>% ()) strings
                expectParity "PString" (bindsUnit ((|PString|_|) "")) (pstring "" >>% ()) strings
                expectParity "PStringCI" (bindsUnit ((|PStringCI|_|) "abc")) (stringCIReturn "abc" ()) strings
                expectParity "AsciiLetter" (|AsciiLetter|_|) asciiLetter strings
                expectParity "Digit" (|Digit|_|) digit strings
                expectParity "Newline" (bindsUnit (|Newline|_|)) skipNewline strings
                expectParity "Spaces" (bindsUnit (|Spaces|_|)) spaces strings
                expectParity "Spaces1" (bindsUnit (|Spaces1|_|)) spaces1 strings
                expectParity "ManyChars" ((|ManyChars|_|) asciiLetter) (manyChars asciiLetter) strings
                expectParity "Many1Chars" ((|Many1Chars|_|) asciiLetter) (many1Chars asciiLetter) strings
                expectParity "Many1Chars2" ((|Many1Chars2|_|) asciiLetter digit) (many1Chars2 asciiLetter digit) strings

                expectParity
                    "ManyCharsTill"
                    ((|ManyCharsTill|_|) anyChar (pchar 'c'))
                    (manyCharsTill anyChar (pchar 'c'))
                    strings
            }

            test "CharParsers: numbers" {
                expectParity "PInt8" (|PInt8|_|) pint8 strings
                expectParity "PInt16" (|PInt16|_|) pint16 strings
                expectParity "PInt32" (|PInt32|_|) pint32 strings
                expectParity "PInt64" (|PInt64|_|) pint64 strings
                expectParity "PUInt8" (|PUInt8|_|) puint8 strings
                expectParity "PUInt16" (|PUInt16|_|) puint16 strings
                expectParity "PUInt32" (|PUInt32|_|) puint32 strings
                expectParity "PUInt64" (|PUInt64|_|) puint64 strings
                expectParity "PBigInt" (|PBigInt|_|) pbigint strings
                expectParity "PFloat" (|PFloat|_|) pfloat strings
            }

            test "CharParsers: any of several strings" {
                let words = [ "let"; "letter"; "x" ]
                let pairs = [ "let", 1; "letter", 2; "x", 3 ]
                let ignoreCase = StringComparison.OrdinalIgnoreCase

                expectParity "AnyString" ((|AnyString|_|) words) (anyString words) strings
                expectParity "AnyStringCI" ((|AnyStringCI|_|) words) (anyStringCI words) strings
                expectParity "AnyStringBy" ((|AnyStringBy|_|) ignoreCase words) (anyStringBy ignoreCase words) strings
                expectParity "AnyStringReturn" ((|AnyStringReturn|_|) pairs) (anyStringReturn pairs) strings
                expectParity "AnyStringCIReturn" ((|AnyStringCIReturn|_|) pairs) (anyStringCIReturn pairs) strings

                expectParity
                    "AnyStringByReturn"
                    ((|AnyStringByReturn|_|) ignoreCase pairs)
                    (anyStringByReturn ignoreCase pairs)
                    strings
            }

            test "Combinators" {
                let minus = pchar '-' >>% (-)

                expectParity
                    "Between"
                    ((|Between|_|) (pchar '(') (pchar ')') digit)
                    (between (pchar '(') (pchar ')') digit)
                    strings

                expectParity "Choice" ((|Choice|_|) [ pchar 'a'; digit ]) (choice [ pchar 'a'; digit ]) strings
                expectParity "Opt" ((|Opt|_|) digit) (opt digit) strings
                expectParity "Optional" (bindsUnit ((|Optional|_|) digit)) (optional digit) strings
                expectParity "NotEmpty" ((|NotEmpty|_|) (manyChars digit)) (notEmpty (manyChars digit)) strings

                expectParity
                    "FollowedBy"
                    (bindsUnit (fun c ->
                        match c with
                        | FollowedBy digit -> ValueSome c
                        | _ -> ValueNone
                    ))
                    (followedBy digit)
                    strings

                expectParity
                    "NotFollowedBy"
                    (bindsUnit (fun c ->
                        match c with
                        | NotFollowedBy digit -> ValueSome c
                        | _ -> ValueNone
                    ))
                    (notFollowedBy digit)
                    strings

                expectParity
                    "LookAhead"
                    (fun c ->
                        match c with
                        | LookAhead digit x -> ValueSome(x, c)
                        | _ -> ValueNone
                    )
                    (lookAhead digit)
                    strings

                expectParityBy items "PArray" ((|PArray|_|) 2 digit) (parray 2 digit) strings
                expectParityBy items "Many" ((|Many|_|) digit) (many digit) strings
                expectParityBy items "Many1" ((|Many1|_|) digit) (many1 digit) strings

                expectParityBy
                    items
                    "Many1Items2"
                    ((|Many1Items2|_|) asciiLetter digit)
                    (many1Items2 asciiLetter digit)
                    strings

                expectParity
                    "CountManySatisfies"
                    ((|CountManySatisfies|_|) Char.IsDigit)
                    (countManySatisfies Char.IsDigit)
                    strings

                expectParity
                    "CountMany1Satisfies"
                    ((|CountMany1Satisfies|_|) Char.IsDigit)
                    (countMany1Satisfies Char.IsDigit)
                    strings

                let both (struct (xs, ys)) = items xs, items ys
                let withEnd (struct (xs, e)) = items xs, e

                expectParityBy both "SepBy" ((|SepBy|_|) digit (pchar ',')) (sepBy digit (pchar ',')) strings
                expectParityBy both "SepBy1" ((|SepBy1|_|) digit (pchar ',')) (sepBy1 digit (pchar ',')) strings
                expectParityBy both "SepEndBy" ((|SepEndBy|_|) digit (pchar ',')) (sepEndBy digit (pchar ',')) strings

                expectParityBy
                    both
                    "SepEndBy1"
                    ((|SepEndBy1|_|) digit (pchar ','))
                    (sepEndBy1 digit (pchar ','))
                    strings

                expectParityBy
                    withEnd
                    "ManyTill"
                    ((|ManyTill|_|) digit (pchar ';'))
                    (manyTill digit (pchar ';'))
                    strings

                expectParityBy
                    withEnd
                    "Many1Till"
                    ((|Many1Till|_|) digit (pchar ';'))
                    (many1Till digit (pchar ';'))
                    strings

                expectParity "Chainl1" ((|Chainl1|_|) pint32 minus) (chainl1 pint32 minus) strings
                expectParity "Chainl" ((|Chainl|_|) pint32 minus 0) (chainl pint32 minus 0) strings
                expectParity "Chainr1" ((|Chainr1|_|) pint32 minus) (chainr1 pint32 minus) strings
                expectParity "Chainr" ((|Chainr|_|) pint32 minus 0) (chainr pint32 minus 0) strings
            }

            test "Combinator patterns nest" {
                // An operator cannot appear in a pattern argument: bind the parser to a name.
                let minus = pchar '-' >>% (-)

                match ActiveParser.ofReader (Reader.ofString "8-3-2;" ()) with
                | Chainl1 pint32 minus (left, PChar ';' EOF) -> "" |> Expect.equal left 3
                | _ -> failtest "Should match a chain and ;"
            }

            test "ByteParsers" {
                expectParity "PFloat16BE" (|PFloat16BE|_|) pFloat16BE bytes
                expectParity "PFloat16LE" (|PFloat16LE|_|) pFloat16LE bytes
                expectParity "PFloat32BE" (|PFloat32BE|_|) pFloat32BE bytes
                expectParity "PFloat32LE" (|PFloat32LE|_|) pFloat32LE bytes
                expectParity "PFloat64BE" (|PFloat64BE|_|) pFloat64BE bytes
                expectParity "PFloat64LE" (|PFloat64LE|_|) pFloat64LE bytes
                expectParity "PUInt16BE" (|PUInt16BE|_|) pUInt16BE bytes
                expectParity "PUInt16LE" (|PUInt16LE|_|) pUInt16LE bytes
                expectParity "PUInt32BE" (|PUInt32BE|_|) pUInt32BE bytes
                expectParity "PUInt32LE" (|PUInt32LE|_|) pUInt32LE bytes
                expectParity "PUInt64BE" (|PUInt64BE|_|) pUInt64BE bytes
                expectParity "PUInt64LE" (|PUInt64LE|_|) pUInt64LE bytes
                expectParity "PUInt128BE" (|PUInt128BE|_|) pUInt128BE bytes
                expectParity "PUInt128LE" (|PUInt128LE|_|) pUInt128LE bytes
                expectParity "PUIntPtrBE" (|PUIntPtrBE|_|) pUIntPtrBE bytes
                expectParity "PUIntPtrLE" (|PUIntPtrLE|_|) pUIntPtrLE bytes
                expectParity "PInt16BE" (|PInt16BE|_|) pInt16BE bytes
                expectParity "PInt16LE" (|PInt16LE|_|) pInt16LE bytes
                expectParity "PInt32BE" (|PInt32BE|_|) pInt32BE bytes
                expectParity "PInt32LE" (|PInt32LE|_|) pInt32LE bytes
                expectParity "PInt64BE" (|PInt64BE|_|) pInt64BE bytes
                expectParity "PInt64LE" (|PInt64LE|_|) pInt64LE bytes
                expectParity "PInt128BE" (|PInt128BE|_|) pInt128BE bytes
                expectParity "PInt128LE" (|PInt128LE|_|) pInt128LE bytes
                expectParity "PIntPtrBE" (|PIntPtrBE|_|) pIntPtrBE bytes
                expectParity "PIntPtrLE" (|PIntPtrLE|_|) pIntPtrLE bytes
            }

            test "Byte patterns nest" {
                let reader = Reader.ofArray [| 0uy; 7uy; 3uy; 0uy; 0uy; 0uy |] ()

                match ActiveParser.ofReader reader with
                | PUInt16BE(tag, PInt32LE(length, EOF)) ->
                    "" |> Expect.equal tag 7us
                    "" |> Expect.equal length 3
                | _ -> failtest "Should match a tag and a length"
            }
        ]
