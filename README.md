# Script-n-Scribe: Prodirect Manipulation of Music

Author: Teddy Sudol
CS 691 NN, Fall 2015

## Installing

Assuming you have GHC, GHCi and cabal installed:

1. Download the code into some directory: `git clone git@github.com:Solumin/ScriptNScribe`
2. `cd` into the project directory
3. (Optional) `cabal sandbox init` to create a safe environment for installing
   packages
4. `cabal install` to download and install dependencies

To uninstall this later, delete the sandbox using `cabal sandbox delete`. (Or
just rm -rf the whole directory.)

## Running

The easiest way to interact with Script-n-Scribe is through GHCi, the Haskell
REPL. Cabal provides its own `repl` command that will automatically load the
library into GHCi. This loads the entire library instead of individual files. To
access the Demo and UI.BreveUI "main" functions, you need to load them
specifically with :load. (e.g. `:l UI.BreveUI` then `:main` will launch the UI.)

The other option is to load the source files directly into GHCi.

From inside the `src` directory, running `ghci <file>` will load that file for
use. Different files load different parts of the project:

- BreveLang.hs for access to the parser functions
- BreveEval.hs: evaluate, run and perform files. Includes BreveLang.hs
- Synth.hs: run synthesis. Includes BreveLang and BreveEval
- Demo.hs: To see run the demo from my presentation. (Run `:main` from inside
  GHCi to run the demo) Includes previous files.

The UI is located in `src/UI`. To run it, cd into that directory and run `ghci
-i".." BreveUI.hs`. Once it has loaded, run `:main` to start the UI, which will
be available on localhost post 8023.

## Dependencies:

- `euterpea` (Hudak's music/sound library, as seen in the Haskell School of
  Music.)
- `parsec` (Applicative parsing library. See src/BreveLang.hs)
- `raw-strings-qq` (QuasiQuoter for raw strings -- see src/BrevePrelude.hs)

### MIDI

[Euterpea][http://haskell.cs.yale.edu/euterpea/download/] has easy instructions
for setting up MIDI playback from Haskell on Windows, Mac and Linux.

## Important Files

- `doc/`: Contains documentation, including the project report, the Breve
  specification and the UI plan.
- `src/`:
  - BreveLang.hs: The parser
  - BreveEval.hs: The evaluator
  - BrevePrelude.hs: The Prelude for Breve programs
  - Synth.hs: The synthesis system
  - Demo.hs: A short demo of Breve. Compile it, or load it into ghci and run
    ":main"
  - `UI/`: Breve UI package
    - BreveUI.hs: The current UI. Somewhat brittle.
    - `static/`: Contains static web content served by the UI, most notably the
      index.html and breve.css files.
- `examples/`: Contains example Breve (.brv) programs. To execute from inside
  ghci: `perform =<< readFile "fileName"`
    - e.g. `perform =<< readFile "../examples/frerejacques.brv"`

## The State of Testing

I am quite disheartened to say that there are currently no tests for this
project. This is partly due to the highly organic nature of this project, where
tests would be invalid immediately after writing them. I also had difficulty
putting tests into the typical QuickCheck paradigm, though I have a better idea
of that now.

The future work section of the paper mentions various parts of Script-n-Scribe
that need to be improved. Adding tests would be one of those areas, but it is a
failing of the project, not of the product. Consider it at the top of the "fix
this project" list.
