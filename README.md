# Breve: Prodirect Manipulation of Music

## Task List:

1. Tracing
1. Better error messages for parsing AND evaluating!
1. Tests for Parser and Evaluation. (Probably can't use quickcheck for this,
   unfortunately.)
1. "Unparsing" for generating programs after tracing.
1. UI for direct manipulation.

### Minor Tasks:

1. Function definition sugar
1. Pattern matching function arguments

### Dependencies:

- Euterpea (Hudak's music/sound library, as seen in the Haskell School of
  Music.)
- Parsec (Applicative parsing library. See src/BreveLang.hs)
- raw-strings-qq (QuasiQuoter for raw strings -- see src/BrevePrelude.hs)
    - Requires QuasiQuotes GHC extension
