# Changelog

## 0.1.0 - 2026-11-18

### 0.1.0 Added

- First release, for XParsec 1.0.0. Write parsers as F# active patterns and match them in `match` expressions: an immutable `Cursor` over a `Reader`, the `ActiveParser` type, a pattern for each parser of `Parsers`, `CharParsers` and `ByteParsers` and for each combinator a `match` does not express (`Item`, `AnyOf`, `PChar`, `PString`, `PInt32`, `PFloat`, `AnyString`, `PUInt16BE`, `Many`, `SepBy`, `Opt`, `Chainl1`, ...) that runs it, `Parsed` and `Skipped` to run any parser, `Until` and `Delimited`, conversions to and from XParsec parsers (`ActiveParser.ofParser`, `toParser`, `toParserL`), `ActiveParser.cursor`/`seek` for `match!` in `parser { }`, and `ActiveParser.unfold` for tokenizers. Ports the techniques of [ActiveParsers](https://github.com/mushedResearch/ActiveParsers) to [XParsec](https://github.com/roboz0r/XParsec).
