namespace XParsec.ActiveParsers

open XParsec

/// Functions that convert between XParsec parsers and active parsers.
[<RequireQualifiedAccess>]
module ActiveParser =

    /// The error of <see cref="toParser"/> when no pattern matched.
    let noMatch<'T, 'State> : ErrorType<'T, 'State> =
        Message "No active pattern matched."

    /// Returns a cursor at the current position of the reader.
    let inline ofReader (reader: Reader<'T, 'State, 'Input>) : Cursor<'T, 'State, 'Input> =
        {
            Reader = reader
            Position = reader.Position
        }

    /// <summary>
    /// Turns an XParsec parser into an active parser: runs <paramref name="p"/> from the cursor
    /// and returns its result with the cursor after it, or <c>ValueNone</c> when it fails.
    /// </summary>
    /// <remarks>The error of a failed parser is dropped; label the parser built with <see cref="toParserL"/> instead.</remarks>
    let inline ofParser
        ([<InlineIfLambda>] p: Parser<'A, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        : ('A * Cursor<'T, 'State, 'Input>) voption =
        let reader = cursor.Reader
        reader.Position <- cursor.Position

        match p reader with
        | Ok x -> ValueSome(x, ofReader reader)
        | Error _ -> ValueNone

    /// <summary>
    /// Turns an active parser into an XParsec parser that fails with <see cref="noMatch"/>
    /// when <paramref name="p"/> does not match. On failure the reader is left where it started.
    /// </summary>
    /// <remarks>
    /// The usual source of <paramref name="p"/> is a function that matches its cursor:
    /// <code>
    /// let token = ActiveParser.toParser (fun cursor ->
    ///     match cursor with
    ///     | Number (n, next) -> ValueSome(Token.Number n, next)
    ///     | PChar '+' next -> ValueSome(Token.Plus, next)
    ///     | _ -> ValueNone)
    /// </code>
    /// </remarks>
    let inline toParser ([<InlineIfLambda>] p: ActiveParser<'A, 'T, 'State, 'Input>) : Parser<'A, 'T, 'State, 'Input> =
        fun reader ->
            let start = reader.Position

            match p (ofReader reader) with
            | ValueSome(x, next) ->
                reader.Position <- next.Position
                Ok x
            | ValueNone ->
                reader.Position <- start
                ParseError.create noMatch start

    /// <summary>
    /// Turns an active parser into an XParsec parser that fails with <paramref name="message"/>
    /// when <paramref name="p"/> does not match. On failure the reader is left where it started.
    /// </summary>
    let inline toParserL
        (message: string)
        ([<InlineIfLambda>] p: ActiveParser<'A, 'T, 'State, 'Input>)
        : Parser<'A, 'T, 'State, 'Input> =
        // `toParser` rewinds on failure, so `<?>` always replaces its error.
        toParser p <?> message

    /// <summary>
    /// A parser that returns a cursor at the current position without consuming input.
    /// With <see cref="seek"/>, it lets a <c>parser { }</c> expression match active patterns:
    /// <code>
    /// parser {
    ///     match! ActiveParser.cursor with
    ///     | PString "let" next ->
    ///         do! ActiveParser.seek next
    ///         ...
    ///     | _ -> return! fail (Message "Expected let")
    /// }
    /// </code>
    /// </summary>
    let inline cursor (reader: Reader<'T, 'State, 'Input>) : ParseResult<Cursor<'T, 'State, 'Input>, 'T, 'State> =
        Ok(ofReader reader)

    /// A parser that moves the reader to the cursor, which must come from the same reader.
    let inline seek
        (cursor: Cursor<'T, 'State, 'Input>)
        (reader: Reader<'T, 'State, 'Input>)
        : ParseResult<unit, 'T, 'State> =
        reader.Position <- cursor.Position
        Ok()

    /// Applies <paramref name="mapping"/> to the value of the active parser <paramref name="p"/>.
    let inline map
        ([<InlineIfLambda>] mapping: 'A -> 'B)
        ([<InlineIfLambda>] p: ActiveParser<'A, 'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        : ('B * Cursor<'T, 'State, 'Input>) voption =
        match p cursor with
        | ValueSome(x, next) -> ValueSome(mapping x, next)
        | ValueNone -> ValueNone

    /// <summary>
    /// Runs <paramref name="step"/> from the cursor, then from the cursor it returns, until it
    /// returns <c>Break</c> or <c>BreakWith</c>, and returns the values of the
    /// <c>ContinueWith</c> and <c>BreakWith</c> steps. Always matches.
    /// </summary>
    /// <remarks>
    /// <para>
    /// This is the shape of a tokenizer written with active patterns:
    /// <code>
    /// let tokens = ActiveParser.unfold (fun cursor ->
    ///     match cursor with
    ///     | EOF -> Break, cursor
    ///     | Spaces1 next -> Continue, next
    ///     | Number (n, next) -> ContinueWith(Token.Number n), next
    ///     | _ -> Break, cursor)
    /// </code>
    /// </para>
    /// <para>
    /// Raises <see cref="InfiniteLoopException{S}"/> when a <c>Continue</c> or <c>ContinueWith</c>
    /// step returns its cursor unchanged, like <c>many</c>.
    /// </para>
    /// </remarks>
    let unfold
        (step: Cursor<'T, 'State, 'Input> -> Unfold<'A> * Cursor<'T, 'State, 'Input>)
        (cursor: Cursor<'T, 'State, 'Input>)
        : (System.Collections.Immutable.ImmutableArray<'A> * Cursor<'T, 'State, 'Input>) voption =
        let mutable xs = SmallArrayBuilder<'A>()
        let mutable current = cursor
        let mutable go = true

        while go do
            let action, next = step current

            match action with
            | Continue
            | ContinueWith _ when next.Position = current.Position -> raise (InfiniteLoopException current.Position)
            | Continue -> ()
            | ContinueWith x -> xs.Add x
            | Break -> go <- false
            | BreakWith x ->
                xs.Add x
                go <- false

            current <- next

        ValueSome(xs.ToImmutable(), current)
