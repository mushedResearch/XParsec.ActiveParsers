namespace XParsec.ActiveParsers

open XParsec

/// <summary>
/// The combinators of <c>XParsec.Combinators</c> as active patterns. Each takes XParsec parsers, as its
/// combinator does, runs the combinator, and binds its value and the cursor after it.
/// </summary>
/// <remarks>
/// <para>
/// The combinators that a <c>match</c> already expresses have no pattern: nest patterns to sequence
/// parsers (<c>&gt;&gt;.</c>, <c>.&gt;&gt;</c>, <c>.&gt;&gt;.</c>, <c>tuple2</c>, <c>pipe2</c>, <c>bind</c>), map the
/// value in the arm (<c>|&gt;&gt;</c>, <c>&gt;&gt;%</c>), write alternatives as arms (<c>&lt;|&gt;</c>,
/// <c>&lt;|&gt;%</c>), and match the next item for <c>dispatch</c>. Patterns carry no error, so the labels
/// (<c>&lt;?&gt;</c>, <c>choiceL</c>, <c>followedByL</c>, ...) have none either; ignore the value of a pattern
/// for the <c>skip...</c> combinators.
/// </para>
/// <para>
/// To run an active parser with a combinator, turn it into a parser with <c>ActiveParser.toParser</c>.
/// </para>
/// </remarks>
[<AutoOpen>]
module CombinatorPatterns =

    /// Matches <c>pOpen</c>, <c>p</c> and <c>pClose</c>, as <c>between</c>; binds the value of <c>p</c>.
    [<return: Struct>]
    let inline (|Between|_|)
        (pOpen: Parser<'A, 'T, 'State, 'Input>)
        (pClose: Parser<'B, 'T, 'State, 'Input>)
        (p: Parser<'C, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (between pOpen pClose p) cursor

    /// Matches the first of the parsers <c>ps</c> that succeeds, as <c>choice</c>.
    [<return: Struct>]
    let inline (|Choice|_|) (ps: Parser<'A, 'T, 'State, 'Input> seq) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (choice ps) cursor

    /// Matches <c>p</c> if it succeeds, as <c>opt</c>; binds <c>ValueSome</c> of its value, or <c>ValueNone</c>
    /// and the cursor unchanged. Always matches.
    [<return: Struct>]
    let inline (|Opt|_|) (p: Parser<'A, 'T, 'State, 'Input>) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (opt p) cursor

    /// Matches <c>p</c> if it succeeds, as <c>optional</c>; binds the cursor after it. Always matches.
    [<return: Struct>]
    let inline (|Optional|_|) (p: Parser<'A, 'T, 'State, 'Input>) (cursor: Cursor<'T, 'State, 'Input>) =
        match cursor with
        | Skipped (optional p) next -> ValueSome next
        | _ -> ValueNone

    /// Matches <c>p</c> when it consumes input, as <c>notEmpty</c>.
    [<return: Struct>]
    let inline (|NotEmpty|_|) (p: Parser<'A, 'T, 'State, 'Input>) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (notEmpty p) cursor

    /// Matches when <c>p</c> would succeed, as <c>followedBy</c>, without consuming input.
    [<return: Struct>]
    let inline (|FollowedBy|_|) (p: Parser<'A, 'T, 'State, 'Input>) (cursor: Cursor<'T, 'State, 'Input>) =
        match cursor with
        | Skipped (followedBy p) _ -> ValueSome()
        | _ -> ValueNone

    /// Matches when <c>p</c> would fail, as <c>notFollowedBy</c>, without consuming input.
    [<return: Struct>]
    let inline (|NotFollowedBy|_|) (p: Parser<'A, 'T, 'State, 'Input>) (cursor: Cursor<'T, 'State, 'Input>) =
        match cursor with
        | Skipped (notFollowedBy p) _ -> ValueSome()
        | _ -> ValueNone

    /// Matches when <c>p</c> would succeed, as <c>lookAhead</c>, without consuming input; binds its value only.
    [<return: Struct>]
    let inline (|LookAhead|_|) (p: Parser<'A, 'T, 'State, 'Input>) (cursor: Cursor<'T, 'State, 'Input>) =
        match ActiveParser.ofParser (lookAhead p) cursor with
        | ValueSome(x, _) -> ValueSome x
        | ValueNone -> ValueNone

    /// Matches <c>p</c> exactly <c>n</c> times, as <c>parray</c>; binds its values.
    [<return: Struct>]
    let inline (|PArray|_|) (n: int) (p: Parser<'A, 'T, 'State, 'Input>) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (parray n p) cursor

    /// Matches <c>p</c> zero or more times, as <c>many</c>; binds its values. Always matches.
    [<return: Struct>]
    let inline (|Many|_|) (p: Parser<'A, 'T, 'State, 'Input>) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (many p) cursor

    /// Matches <c>p</c> one or more times, as <c>many1</c>; binds its values.
    [<return: Struct>]
    let inline (|Many1|_|) (p: Parser<'A, 'T, 'State, 'Input>) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (many1 p) cursor

    /// Matches <c>p1</c>, then <c>p</c> zero or more times, as <c>many1Items2</c>; binds their values.
    [<return: Struct>]
    let inline (|Many1Items2|_|)
        (p1: Parser<'A, 'T, 'State, 'Input>)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (many1Items2 p1 p) cursor

    /// Matches zero or more items that satisfy <c>predicate</c>, as <c>countManySatisfies</c>; binds their
    /// count. Always matches.
    [<return: Struct>]
    let inline (|CountManySatisfies|_|) (predicate: 'T -> bool) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (countManySatisfies predicate) cursor

    /// Matches one or more items that satisfy <c>predicate</c>, as <c>countMany1Satisfies</c>; binds their count.
    [<return: Struct>]
    let inline (|CountMany1Satisfies|_|) (predicate: 'T -> bool) (cursor: Cursor<'T, 'State, 'Input>) =
        ActiveParser.ofParser (countMany1Satisfies predicate) cursor

    /// Matches <c>p</c> zero or more times separated by <c>pSep</c>, as <c>sepBy</c>; binds
    /// <c>struct (values, separators)</c>. Always matches.
    [<return: Struct>]
    let inline (|SepBy|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pSep: Parser<'B, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (sepBy p pSep) cursor

    /// Matches <c>p</c> one or more times separated by <c>pSep</c>, as <c>sepBy1</c>; binds
    /// <c>struct (values, separators)</c>.
    [<return: Struct>]
    let inline (|SepBy1|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pSep: Parser<'B, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (sepBy1 p pSep) cursor

    /// Matches <c>p</c> zero or more times separated and optionally ended by <c>pSep</c>, as <c>sepEndBy</c>;
    /// binds <c>struct (values, separators)</c>. Always matches.
    [<return: Struct>]
    let inline (|SepEndBy|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pSep: Parser<'B, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (sepEndBy p pSep) cursor

    /// Matches <c>p</c> one or more times separated and optionally ended by <c>pSep</c>, as <c>sepEndBy1</c>;
    /// binds <c>struct (values, separators)</c>.
    [<return: Struct>]
    let inline (|SepEndBy1|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pSep: Parser<'B, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (sepEndBy1 p pSep) cursor

    /// Matches <c>p</c> zero or more times until <c>pEnd</c>, as <c>manyTill</c>; binds
    /// <c>struct (values, endValue)</c>.
    [<return: Struct>]
    let inline (|ManyTill|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pEnd: Parser<'B, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (manyTill p pEnd) cursor

    /// Matches <c>p</c> one or more times until <c>pEnd</c>, as <c>many1Till</c>; binds
    /// <c>struct (values, endValue)</c>.
    [<return: Struct>]
    let inline (|Many1Till|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pEnd: Parser<'B, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (many1Till p pEnd) cursor

    /// Matches <c>p</c> one or more times separated by <c>pOp</c>, as <c>chainl1</c>; binds the values
    /// combined by the operators, left-associative.
    [<return: Struct>]
    let inline (|Chainl1|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pOp: Parser<'A -> 'A -> 'A, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (chainl1 p pOp) cursor

    /// As <c>Chainl1</c>, or binds <c>orElse</c> when <c>p</c> fails at once, as <c>chainl</c>. Always matches.
    [<return: Struct>]
    let inline (|Chainl|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pOp: Parser<'A -> 'A -> 'A, 'T, 'State, 'Input>)
        (orElse: 'A)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (chainl p pOp orElse) cursor

    /// Matches <c>p</c> one or more times separated by <c>pOp</c>, as <c>chainr1</c>; binds the values
    /// combined by the operators, right-associative.
    [<return: Struct>]
    let inline (|Chainr1|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pOp: Parser<'A -> 'A -> 'A, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (chainr1 p pOp) cursor

    /// As <c>Chainr1</c>, or binds <c>orElse</c> when <c>p</c> fails at once, as <c>chainr</c>. Always matches.
    [<return: Struct>]
    let inline (|Chainr|_|)
        (p: Parser<'A, 'T, 'State, 'Input>)
        (pOp: Parser<'A -> 'A -> 'A, 'T, 'State, 'Input>)
        (orElse: 'A)
        (cursor: Cursor<'T, 'State, 'Input>)
        =
        ActiveParser.ofParser (chainr p pOp orElse) cursor
