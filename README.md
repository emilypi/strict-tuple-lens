# strict-tuple-lens
![Build Status](https://github.com/emilypi/strict-tuple-lens/workflows/CI/badge.svg)
[![Hackage](https://img.shields.io/hackage/v/strict-tuple-lens.svg)](https://hackage.haskell.org/package/strict-tuple-lens)

Optics and instances for the `strict-tuple` library. The following instances are defined:

- [`Field`](https://github.com/emilypi/strict-tuple-lens/blob/master/src/Data/Tuple/Strict/Lens/Field.hs) instances for `T1` through `T19`.
- [`Each`](https://github.com/emilypi/strict-tuple-lens/blob/master/src/Data/Tuple/Strict/Lens/Each.hs) instances for  `T1` through `T9`.
- [`AsEmpty`](https://github.com/emilypi/strict-tuple-lens/blob/master/src/Data/Tuple/Strict/Lens/Empty.hs) instances for `T2` and `T3`

This is the dedicated place for any `lens` orphans written for the `strict-tuple` library.
