namespace XParsec.ActiveParsers

#if !FABLE_COMPILER
open XParsec

/// <summary>
/// Active patterns over a <see cref="Cursor{T,S,I}"/> of bytes: the parsers of <c>XParsec.ByteParsers</c>
/// as patterns. Each binds the number and the cursor after its bytes:
/// <code>
/// match cursor with
/// | PUInt16BE(tag, PInt32LE(length, next)) -> ...
/// </code>
/// </summary>
/// <remarks>
/// As <c>ByteParsers</c>, they are not available with Fable, and the floating-point, 128-bit and
/// native-sized integer patterns need .NET 8 or later.
/// </remarks>
[<AutoOpen>]
module BytePatterns =

#if NET8_0_OR_GREATER
    /// Matches a 16-bit floating-point number in big-endian format, as <c>pFloat16BE</c>.
    [<return: Struct>]
    let inline (|PFloat16BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pFloat16BE cursor

    /// Matches a 16-bit floating-point number in little-endian format, as <c>pFloat16LE</c>.
    [<return: Struct>]
    let inline (|PFloat16LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pFloat16LE cursor

    /// Matches a 32-bit floating-point number in big-endian format, as <c>pFloat32BE</c>.
    [<return: Struct>]
    let inline (|PFloat32BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pFloat32BE cursor

    /// Matches a 32-bit floating-point number in little-endian format, as <c>pFloat32LE</c>.
    [<return: Struct>]
    let inline (|PFloat32LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pFloat32LE cursor

    /// Matches a 64-bit floating-point number in big-endian format, as <c>pFloat64BE</c>.
    [<return: Struct>]
    let inline (|PFloat64BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pFloat64BE cursor

    /// Matches a 64-bit floating-point number in little-endian format, as <c>pFloat64LE</c>.
    [<return: Struct>]
    let inline (|PFloat64LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pFloat64LE cursor
#endif

    /// Matches an unsigned 16-bit integer in big-endian format, as <c>pUInt16BE</c>.
    [<return: Struct>]
    let inline (|PUInt16BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUInt16BE cursor

    /// Matches an unsigned 16-bit integer in little-endian format, as <c>pUInt16LE</c>.
    [<return: Struct>]
    let inline (|PUInt16LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUInt16LE cursor

    /// Matches an unsigned 32-bit integer in big-endian format, as <c>pUInt32BE</c>.
    [<return: Struct>]
    let inline (|PUInt32BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUInt32BE cursor

    /// Matches an unsigned 32-bit integer in little-endian format, as <c>pUInt32LE</c>.
    [<return: Struct>]
    let inline (|PUInt32LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUInt32LE cursor

    /// Matches an unsigned 64-bit integer in big-endian format, as <c>pUInt64BE</c>.
    [<return: Struct>]
    let inline (|PUInt64BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUInt64BE cursor

    /// Matches an unsigned 64-bit integer in little-endian format, as <c>pUInt64LE</c>.
    [<return: Struct>]
    let inline (|PUInt64LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUInt64LE cursor

#if NET8_0_OR_GREATER
    /// Matches an unsigned 128-bit integer in big-endian format, as <c>pUInt128BE</c>.
    [<return: Struct>]
    let inline (|PUInt128BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUInt128BE cursor

    /// Matches an unsigned 128-bit integer in little-endian format, as <c>pUInt128LE</c>.
    [<return: Struct>]
    let inline (|PUInt128LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUInt128LE cursor

    /// Matches an unsigned native-sized integer in big-endian format, as <c>pUIntPtrBE</c>.
    [<return: Struct>]
    let inline (|PUIntPtrBE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUIntPtrBE cursor

    /// Matches an unsigned native-sized integer in little-endian format, as <c>pUIntPtrLE</c>.
    [<return: Struct>]
    let inline (|PUIntPtrLE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pUIntPtrLE cursor
#endif

    /// Matches a signed 16-bit integer in big-endian format, as <c>pInt16BE</c>.
    [<return: Struct>]
    let inline (|PInt16BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pInt16BE cursor

    /// Matches a signed 16-bit integer in little-endian format, as <c>pInt16LE</c>.
    [<return: Struct>]
    let inline (|PInt16LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pInt16LE cursor

    /// Matches a signed 32-bit integer in big-endian format, as <c>pInt32BE</c>.
    [<return: Struct>]
    let inline (|PInt32BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pInt32BE cursor

    /// Matches a signed 32-bit integer in little-endian format, as <c>pInt32LE</c>.
    [<return: Struct>]
    let inline (|PInt32LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pInt32LE cursor

    /// Matches a signed 64-bit integer in big-endian format, as <c>pInt64BE</c>.
    [<return: Struct>]
    let inline (|PInt64BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pInt64BE cursor

    /// Matches a signed 64-bit integer in little-endian format, as <c>pInt64LE</c>.
    [<return: Struct>]
    let inline (|PInt64LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pInt64LE cursor

#if NET8_0_OR_GREATER
    /// Matches a signed 128-bit integer in big-endian format, as <c>pInt128BE</c>.
    [<return: Struct>]
    let inline (|PInt128BE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pInt128BE cursor

    /// Matches a signed 128-bit integer in little-endian format, as <c>pInt128LE</c>.
    [<return: Struct>]
    let inline (|PInt128LE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pInt128LE cursor

    /// Matches a signed native-sized integer in big-endian format, as <c>pIntPtrBE</c>.
    [<return: Struct>]
    let inline (|PIntPtrBE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pIntPtrBE cursor

    /// Matches a signed native-sized integer in little-endian format, as <c>pIntPtrLE</c>.
    [<return: Struct>]
    let inline (|PIntPtrLE|_|) (cursor: Cursor<byte, 'State, 'Input>) =
        ActiveParser.ofParser ByteParsers.pIntPtrLE cursor
#endif
#endif
