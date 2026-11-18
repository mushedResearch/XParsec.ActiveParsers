namespace XParsec.ActiveParsers

open XParsec

/// <summary>
/// Active patterns over a <see cref="Cursor{T,S,I}"/> for any item type: the parsers of
/// <c>XParsec.Parsers</c> as patterns, and patterns to run other parsers.
/// </summary>
/// <remarks>
/// <para>
/// A pattern that parses a value binds the value and the cursor after it, <c>Name (value, next)</c>;
/// a pattern that only consumes input binds the cursor after it, <c>Name next</c>. Nest a pattern in
/// <c>next</c> to parse what follows:
/// <code>
/// | Item '(' (Parsed expr (e, Item ')' next)) -> ValueSome(e, next)
/// </code>
/// </para>
/// <para>
/// Each pattern named after an XParsec parser runs that parser, so it matches exactly what the parser
/// accepts. The <c>...Return</c>, <c>...L</c> and <c>skip...</c> variants have no pattern of their own:
/// return the value from the arm, label the parser built with <c>ActiveParser.toParserL</c>, and ignore
/// the value bound by the pattern.
/// </para>
/// </remarks>
[<AutoOpen>]
module Patterns =

    /// Matches when the XParsec parser <c>p</c> succeeds; binds its value and the cursor after it.
    [<return: Struct>]
    let inline (|Parsed|_|)
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser p cursor

    /// Matches when the XParsec parser <c>p</c> succeeds; binds the cursor after it.
    [<return: Struct>]
    let inline (|Skipped|_|)
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        match ActiveParser.ofParser p cursor with
        | ValueSome(_, next) -> ValueSome next
        | ValueNone -> ValueNone

    /// Matches when the active parser <c>p</c> matches; binds its value and the cursor after it.
    [<return: Struct>]
    let inline (|Active|_|)
        ([<InlineIfLambda>] p: ActiveParser<'A, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        p cursor

    /// Matches the end of the input, as <c>eof</c>.
    [<return: Struct>]
    let inline (|EOF|_|) (cursor: Cursor<'T, 'State, 'Input>) =
        match cursor with
        | Skipped Parsers.eof _ -> ValueSome()
        | _ -> ValueNone

    /// Matches any item, as <c>pid</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|AnyItem|_|) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser Parsers.pid cursor

    /// Matches the item <c>x</c>, as <c>pitem</c>; binds the cursor after it.
    [<return: Struct>]
    let inline (|Item|_|) (x: 'T) (cursor: Cursor<'T, 'State, 'Input>) =
        match cursor with
        | Skipped (Parsers.skipItem x) next -> ValueSome next
        | _ -> ValueNone

    /// Matches an item that satisfies <c>predicate</c>, as <c>satisfy</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|Satisfy|_|) ([<InlineIfLambda>] predicate: 'T -> bool) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (Parsers.satisfy predicate) cursor

    /// Matches one of the items <c>xs</c>, as <c>anyOf</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|AnyOf|_|) (xs: 'T seq) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (Parsers.anyOf xs) cursor

    /// Matches an item that is none of <c>xs</c>, as <c>noneOf</c>; binds it and the cursor after it.
    [<return: Struct>]
    let inline (|NoneOf|_|) (xs: 'T seq) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (Parsers.noneOf xs) cursor

    /// Matches an item between <c>xMin</c> and <c>xMax</c> inclusive, as <c>anyInRange</c>; binds it and
    /// the cursor after it.
    [<return: Struct>]
    let inline (|InRange|_|) (xMin: 'T) (xMax: 'T) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (Parsers.anyInRange xMin xMax) cursor

    /// Matches the items of <c>xs</c> in order, as <c>pseq</c>; binds the cursor after them. An empty
    /// <c>xs</c> always matches.
    [<return: Struct>]
    let inline (|ItemSeq|_|) (xs: 'T seq) (cursor: Cursor<'T, 'State, 'Input>) =
        match cursor with
        | Skipped (Parsers.pseq xs) next -> ValueSome next
        | _ -> ValueNone

    /// Binds the user state at the cursor, as <c>getUserState</c>. Always matches.
    let (|UserState|) (cursor: Cursor<'T, 'State, 'Input>) = cursor.State

    /// Matches when the user state satisfies <c>predicate</c>, as <c>userStateSatisfies</c>.
    [<return: Struct>]
    let inline (|UserStateSatisfies|_|)
        ([<InlineIfLambda>] predicate: 'State -> bool)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        match cursor with
        | Skipped (Parsers.userStateSatisfies predicate) _ -> ValueSome()
        | _ -> ValueNone

    /// Binds the cursor with the user state <c>state</c>, as <c>setUserState</c>. Always matches.
    [<return: Struct>]
    let inline (|SetUserState|_|) (state: 'State) (cursor: Cursor<'T, 'State, 'Input>) =
        match cursor with
        | Skipped (Parsers.setUserState state) next -> ValueSome next
        | _ -> ValueNone

    /// Binds the cursor with the user state changed by <c>mapping</c>, as <c>updateUserState</c>. Always matches.
    [<return: Struct>]
    let inline (|UpdateUserState|_|)
        ([<InlineIfLambda>] mapping: 'State -> 'State)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        match cursor with
        | Skipped (Parsers.updateUserState mapping) next -> ValueSome next
        | _ -> ValueNone
