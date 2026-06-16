# Changelog

## 0.13.3.0

- Fixed `Show`/`Read` mismatch for `Point`, `Rect` and `Range` by adding
  `showsPrec` implementations that parenthesise values when precedence is
  greater than function-application precedence.
- Added `Read` instances for `Point` and `Rect`.
