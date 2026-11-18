namespace XParsec.ActiveParsers

open System
open System.Text
open XParsec

/// <summary>
/// Active patterns over a <see cref="Cursor{T,S,I}"/> of characters: the parsers of
/// <c>XParsec.CharParsers</c> as patterns, and <c>Until</c> and <c>Delimited</c>.
/// </summary>
/// <remarks>
/// <para>
/// Use them as the arms of a <c>match</c>, nesting a pattern in the cursor another binds:
/// <code>
/// match cursor with
/// | PString "//" (Until "\n" (comment, next)) -> ...
/// | PChar '"' (Until "\"" (text, next)) -> ...
/// </code>
/// </para>
/// <para>
/// A pattern's arguments are evaluated each time the pattern is tried. The <c>AnyString...</c>
/// patterns build a lookup from their strings each time; in a hot loop, build the parser once with
/// <c>CharParsers.anyString</c> and match it with <c>Parsed</c>.
/// </para>
/// </remarks>
[<AutoOpen>]
module CharPatterns =

    /// Matches the character <c>c</c>, as <c>pchar</c>; binds the cursor after it.
    [<return: Struct>]
    let inline (|PChar|_|) (c: char) (cursor: Cursor<char, 'State, 'Input>) =
        match cursor with
        | Skipped (CharParsers.skipChar c) next -> ValueSome next
        | _ -> ValueNone

    /// Matches any character, as <c>anyChar</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|AnyChar|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.anyChar cursor

    /// Matches the string <c>s</c>, as <c>pstring</c>; binds the cursor after it. An empty <c>s</c> always matches.
    [<return: Struct>]
    let inline (|PString|_|) (s: string) (cursor: Cursor<char, 'State, 'Input>) =
        match cursor with
        | Skipped (CharParsers.pstring s) next -> ValueSome next
        | _ -> ValueNone

    /// Matches the string <c>s</c> ignoring case, as <c>stringCIReturn</c>; binds the cursor after it.
    [<return: Struct>]
    let inline (|PStringCI|_|) (s: string) (cursor: Cursor<char, 'State, 'Input>) =
        match cursor with
        | Skipped (CharParsers.stringCIReturn s ()) next -> ValueSome next
        | _ -> ValueNone

    /// Matches an ASCII letter, as <c>asciiLetter</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|AsciiLetter|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.asciiLetter cursor

    /// Matches an ASCII digit, as <c>digit</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|Digit|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.digit cursor

    /// Matches <c>\n</c>, <c>\r</c> or <c>\r\n</c>, as <c>newline</c>; binds the cursor after it.
    [<return: Struct>]
    let inline (|Newline|_|) (cursor: Cursor<char, 'State, 'Input>) =
        match cursor with
        | Skipped CharParsers.skipNewline next -> ValueSome next
        | _ -> ValueNone

    /// Matches zero or more whitespace characters, as <c>spaces</c>; binds the cursor after them.
    [<return: Struct>]
    let inline (|Spaces|_|) (cursor: Cursor<char, 'State, 'Input>) =
        match cursor with
        | Skipped CharParsers.spaces next -> ValueSome next
        | _ -> ValueNone

    /// Matches one or more whitespace characters, as <c>spaces1</c>; binds the cursor after them.
    [<return: Struct>]
    let inline (|Spaces1|_|) (cursor: Cursor<char, 'State, 'Input>) =
        match cursor with
        | Skipped CharParsers.spaces1 next -> ValueSome next
        | _ -> ValueNone

    /// Matches the parser <c>p</c> zero or more times, as <c>manyChars</c>; binds the characters as a
    /// string and the cursor after them.
    [<return: Struct>]
    let inline (|ManyChars|_|) (p: Parser<char, char, 'State, 'Input>) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser (CharParsers.manyChars p) cursor

    /// Matches the parser <c>p</c> one or more times, as <c>many1Chars</c>; binds the characters as a
    /// string and the cursor after them.
    [<return: Struct>]
    let inline (|Many1Chars|_|) (p: Parser<char, char, 'State, 'Input>) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser (CharParsers.many1Chars p) cursor

    /// Matches the parser <c>first</c>, then <c>p</c> zero or more times, as <c>many1Chars2</c>; binds the
    /// characters as a string and the cursor after them.
    [<return: Struct>]
    let inline (|Many1Chars2|_|)
        (first: Parser<char, char, 'State, 'Input>)
        (p: Parser<char, char, 'State, 'Input>)
        (cursor: Cursor<char, 'State, 'Input>)
        =
        ActiveParser.ofParser (CharParsers.many1Chars2 first p) cursor

    /// Matches the parser <c>p</c> until the parser <c>pEnd</c> matches, as <c>manyCharsTill</c>; binds the
    /// characters as a string with the value of <c>pEnd</c>, and the cursor after <c>pEnd</c>.
    [<return: Struct>]
    let inline (|ManyCharsTill|_|)
        (p: Parser<'A, char, 'State, 'Input>)
        (pEnd: Parser<'B, char, 'State, 'Input>)
        (cursor: Cursor<char, 'State, 'Input>)
        =
        ActiveParser.ofParser (CharParsers.manyCharsTill p pEnd) cursor

    /// Matches a signed 8-bit integer, as <c>pint8</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PInt8|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.pint8 cursor

    /// Matches a signed 16-bit integer, as <c>pint16</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PInt16|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.pint16 cursor

    /// Matches a signed 32-bit integer, as <c>pint32</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PInt32|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.pint32 cursor

    /// Matches a signed 64-bit integer, as <c>pint64</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PInt64|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.pint64 cursor

    /// Matches an unsigned 8-bit integer, as <c>puint8</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PUInt8|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.puint8 cursor

    /// Matches an unsigned 16-bit integer, as <c>puint16</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PUInt16|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.puint16 cursor

    /// Matches an unsigned 32-bit integer, as <c>puint32</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PUInt32|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.puint32 cursor

    /// Matches an unsigned 64-bit integer, as <c>puint64</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PUInt64|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.puint64 cursor

    /// Matches an integer of any size, as <c>pbigint</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PBigInt|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.pbigint cursor

    /// Matches a floating-point number, as <c>pfloat</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|PFloat|_|) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser CharParsers.pfloat cursor

    /// Matches the longest of the strings <c>xs</c>, as <c>anyString</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|AnyString|_|) (xs: string seq) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser (CharParsers.anyString xs) cursor

    /// Matches the longest of the strings <c>xs</c> ignoring case, as <c>anyStringCI</c>; binds the string
    /// of <c>xs</c> and the cursor after it.
    [<return: Struct>]
    let inline (|AnyStringCI|_|) (xs: string seq) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser (CharParsers.anyStringCI xs) cursor

    /// Matches the longest of the strings <c>xs</c> compared with <c>comparison</c>, as <c>anyStringBy</c>;
    /// binds the string of <c>xs</c> and the cursor after it.
    [<return: Struct>]
    let inline (|AnyStringBy|_|)
        (comparison: StringComparison)
        (xs: string seq)
        (cursor: Cursor<char, 'State, 'Input>)
        =
        ActiveParser.ofParser (CharParsers.anyStringBy comparison xs) cursor

    /// Matches the longest of the strings of <c>xs</c>, as <c>anyStringReturn</c>; binds the value paired
    /// with it and the cursor after it.
    [<return: Struct>]
    let inline (|AnyStringReturn|_|) (xs: (string * 'A) seq) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser (CharParsers.anyStringReturn xs) cursor

    /// Matches the longest of the strings of <c>xs</c> ignoring case, as <c>anyStringCIReturn</c>; binds the
    /// value paired with it and the cursor after it.
    [<return: Struct>]
    let inline (|AnyStringCIReturn|_|) (xs: (string * 'A) seq) (cursor: Cursor<char, 'State, 'Input>) =
        ActiveParser.ofParser (CharParsers.anyStringCIReturn xs) cursor

    /// Matches the longest of the strings of <c>xs</c> compared with <c>comparison</c>, as
    /// <c>anyStringByReturn</c>; binds the value paired with it and the cursor after it.
    [<return: Struct>]
    let inline (|AnyStringByReturn|_|)
        (comparison: StringComparison)
        (xs: (string * 'A) seq)
        (cursor: Cursor<char, 'State, 'Input>)
        =
        ActiveParser.ofParser (CharParsers.anyStringByReturn comparison xs) cursor

    /// <summary>
    /// Matches the text up to the first <c>delimiter</c> and the delimiter; binds the text without
    /// the delimiter and the cursor after the delimiter. Does not match when the delimiter is missing.
    /// </summary>
    [<return: Struct>]
    let (|Until|_|) (delimiter: string) (cursor: Cursor<char, 'State, 'Input>) =
        let input = cursor.Reader.Input
        let start = cursor.Index
        let last = input.Length - delimiter.Length
        let mutable i = start
        let mutable found = -1

        while found < 0 && i <= last do
            let mutable j = 0

            while j < delimiter.Length && input.[i + j] = delimiter.[j] do
                j <- j + 1

            if j = delimiter.Length then found <- i else i <- i + 1

        if found < 0 then
            ValueNone
        else
            let text = StringBuilder(found - start)

            for k in start .. found - 1 do
                text.Append(input.[k]) |> ignore

            ValueSome(text.ToString(), cursor.Advance(found - start + delimiter.Length))

    /// <summary>
    /// Matches <c>delimiter</c>, the text up to the next <c>delimiter</c>, and that delimiter; binds the
    /// text between them and the cursor after the closing delimiter.
    /// </summary>
    [<return: Struct>]
    let (|Delimited|_|) (delimiter: string) (cursor: Cursor<char, 'State, 'Input>) =
        match cursor with
        | PString delimiter (Until delimiter result) -> ValueSome result
        | _ -> ValueNone
