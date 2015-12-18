module Demo where

import BreveLang
import BreveEval
import Synth

import qualified Data.Map.Lazy as Map
import Data.Maybe (fromJust)

-- Sample program:
-- Define the program
-- Parse it to get the statements
-- eval it to get the traces
-- Make an update for it
-- Synthesize!!
-- Woops, didn't get what we expected
-- Why? Synth choose limited subset of variables to try to solve. Synthesis
--   itself works, but input to it is limited at the moment
-- Play original program, play new program
-- We can undo it!
-- Perform restored

fibo = "fibo = (\\ n -> if n <= 1 then n else fibo(n-1) + fibo(n-2)); main = fibo(5);"

program = "dfsa = arpeggio([0,4,7], (D 4 1/2));\nmain = line(dfsa) :+: (rest 1/4) :+: chord(dfsa);"

states = parse program

traces = snd $ parseEval program

showAll :: Show a => [a] -> IO ()
showAll = mapM_ print

update = [Vp (read "F") (TrOp Add (TrLoc (1,29)) (TrLoc (1,21)))
         ,Vn 4 (TrLoc (1,34))]


synthed = fromJust $ synthesize Faithful AdHoc traces update
-- This returns [TraceMap]. We need to combine them to get the full substitution
-- that combines the updates.
-- combos = map Map.fromList $ sequence $ map Map.toList synthed
combos = map Map.fromList $ mapM Map.toList synthed

newprog = map (show . (`updateProgram` states)) combos

-- When this Demo was written, Synth would return the C# chord version of the
-- synthesized program first. Thanks to the Ad Hoc ranking, it now returns the D
-- minor chord version first instead.
-- To preserve the output of the Demo as it once was, I've rewritten the
-- following few lines to "restore" only the C# chord version.
-- Restore gives us two programs: The original (D major chord) and a C# sus 4
-- chord. Neat.

newState = parse (newprog !! 1)

newTraces = snd $ parseEval (newprog !! 1)

undo = [Vp (read "Fs") (TrOp Add (TrLoc (1,32)) (TrLoc (1,22)))]

restoringUpdates = map Map.fromList $ mapM Map.toList $ fromJust $ synthesize Faithful AdHoc newTraces undo

restored = map (show . (`updateProgram` newState)) restoringUpdates

main = do
    putStrLn "The original program:"
    putStrLn program
    getLine
    perform program
    getLine
    putStrLn "The traces of the program: (numeric and expression)"
    showAll traces
    getLine
    putStrLn "The update we want: Turn the F# into an F"
    print update
    getLine
    putStrLn "Here's the results of synthesis:"
    mapM_ (\p -> putStrLn p >> getLine >> perform p >> getLine) newprog
    putStrLn "\nTwo different programs! One does what we expect (changes the 4 into a 3, giving us a D minor chord)"
    putStrLn "The other turns the root note (D) into a C#, giving us a C# major chord!"
    putStrLn "You'll notice both have quarter notes (1/4) instead of half notes (1/2)"
    putStrLn "\nHere's the original program for comparison:"
    putStrLn program
    getLine
    putStrLn "Synthesis can be undone, too."
    print undo
    putStrLn "This update will turn the F back into an F#."
    getLine
    putStrLn "Here's the results:"
    mapM_ putStrLn restored
    putStrLn "The first one changes the 4 into a 5, and keeps the C# root note. (C# sus. 4 chord! Crunchy.)"
    putStrLn "The synthesis ranking put this one first because it only changes 2 values: the 4 -> 5 and the resulting C# + 5 -> F#"
    putStrLn "The second program has a D instead of a C#, giving us the original program back."
    putStrLn "Three values are changed: D + 0 -> D, D + 4 -> F# and D + 7 -> A."
    getLine
    mapM_ (\p -> putStrLn p >> getLine >> perform p >> getLine) restored
