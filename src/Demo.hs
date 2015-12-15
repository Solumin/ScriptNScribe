import BreveLang
import BreveEval
import Synth

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
showAll = mapM_ (putStrLn . show)

update = [Vp (read "F") (TrOp Add (TrLoc (1,29)) (TrLoc (1,21)))
         ,Vn 4 (TrLoc (1,34))]


synthed = synthFaithful (toTraceMap traces) update

newprog = map (show . (`updateProgram` states)) synthed

states' = map parse newprog

traces' = map (snd . parseEval) newprog

undo = [Vp (read "Fs") (TrOp Add (TrLoc (1,32)) (TrLoc (1,22)))]

restored = map show $ map (`updateProgram` (head states')) $ synthFaithful (toTraceMap $ head traces') undo

main = do
    putStrLn program
    getLine
    perform program
    getLine
    showAll traces
    getLine
    putStrLn $ show update
    getLine
    mapM_ (\p -> putStrLn p >> getLine >> perform p >> getLine) newprog
    putStrLn program
    getLine
    putStrLn $ show undo
    getLine
    mapM_ (\p -> putStrLn p >> getLine >> perform p >> getLine) restored
