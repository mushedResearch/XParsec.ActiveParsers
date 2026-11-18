namespace XParsec.ActiveParsers

open XParsec

/// <summary>
/// An immutable view of a <see cref="Reader{T,S,I}"/> at a fixed <see cref="Position{S}"/>.
/// </summary>
/// <remarks>
/// <para>
/// A <c>Reader</c> is a mutable cursor, but the arms of an F# <c>match</c> must all start
/// from the same input. Active parsers therefore take a <c>Cursor</c> and, on success,
/// return the value they parsed together with the <c>Cursor</c> after it. A failed
/// pattern leaves its input untouched, so nested and alternative patterns backtrack
/// safely, the same way <c>&lt;|&gt;</c> does.
/// </para>
/// <para>
/// The position includes the user state, so state changes made by a pattern that
/// does not match are rolled back too.
/// </para>
/// </remarks>
[<Struct; NoEquality; NoComparison>]
type Cursor<'T, 'State, 'Input when 'Input :> IReadable<'T, 'Input>> =
    {
        Reader: Reader<'T, 'State, 'Input>
        Position: Position<'State>
    }

    /// The index of the cursor in the input.
    member c.Index = c.Position.Index

    /// The user state at the cursor.
    member c.State = c.Position.State

    /// True when the cursor is at the end of the input.
    member c.AtEnd = c.Position.Index >= c.Reader.Length

    /// The item at the cursor, or <c>ValueNone</c> at the end of the input.
    member c.Peek() = c.Reader.Input.TryItem c.Position.Index

    /// The cursor <paramref name="count"/> items further on. Does not check the bounds of the input.
    member c.Advance(count) =
        {
            Reader = c.Reader
            Position =
                { c.Position with
                    Index = c.Position.Index + count
                }
        }

/// <summary>
/// A parser that can be used as a partial active pattern: it returns the parsed value and the
/// cursor after it, or <c>ValueNone</c> when it does not match.
/// </summary>
/// <remarks>
/// Any function of this shape marked <c>[&lt;return: Struct&gt;]</c> is an active pattern:
/// <code>
/// [&lt;return: Struct&gt;]
/// let (|Number|_|) cursor = ActiveParser.ofParser pint32 cursor
///
/// match cursor with
/// | Number (n, next) -> ...
/// </code>
/// </remarks>
type ActiveParser<'Parsed, 'T, 'State, 'Input when 'Input :> IReadable<'T, 'Input>> =
    Cursor<'T, 'State, 'Input> -> ('Parsed * Cursor<'T, 'State, 'Input>) voption

/// <summary>
/// What a step of <see cref="ActiveParser.unfold"/> asks for: go on or stop, with or without a value.
/// </summary>
[<Struct>]
type Unfold<'A> =
    /// Go on without adding a value, for example after skipping whitespace or a comment.
    | Continue
    /// Add the value and go on.
    | ContinueWith of continueWith: 'A
    /// Stop without adding a value.
    | Break
    /// Add the value and stop.
    | BreakWith of breakWith: 'A
