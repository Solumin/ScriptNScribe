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

program = "dfsa = arpeggio([0,4,7], (D 4 1/2)); main = line(dfsa) :+: (rest 1/4) :+: chord(dfsa);"

states = fst $ parse program

traces = snd $ parseEval program

showAll :: Show a => [a] -> IO ()
showAll = mapM_ (putStrLn . show)

update = [Vp (read "F") (TrOp Add (TrLoc (1,29)) (TrLoc (1,21)))]

synthed = fromJust $ synthFaithful (toTraceMap traces) update

newprog = show $ updateProgram synthed states

states' = fst $ parse newprog

traces' = snd $ parseEval newprog

undo = [Vp (read "Fs") (TrOp Add (TrLoc (1,32)) (TrLoc (1,22)))]

restored = show $ (flip updateProgram) states' $ fromJust $ synthFaithful (toTraceMap traces') undo
