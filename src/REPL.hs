import BreveEval
import BreveLang
import BrevePrinter
import Synth
import System.Environment
import qualified Euterpea.Music.Note.Music as E

promptChanges :: IO [String]
promptChanges = putStrLn ">>Enter changes: (line, col) val (blank line when done)" >> changes []

changes :: [String] -> IO [String]
changes ts = do
    -- putStr "> "
    s <- getLine
    case s of
        [] -> return ts
        t -> changes (t:ts)

changesToTraces :: [String] -> Traces
changesToTraces = map parseTrace

parseTrace :: String -> Expr
parseTrace s = let [(loc,rem)] = readLoc s in
    if readPitchClass rem /= [] then (PitchClass (fst $ head $ readPitchClass rem) loc)
    else if readDuration rem /= [] then (Duration (fst $ head $ readDuration rem) loc)
    else if readOctave rem /= [] then (Octave (fst $ head $ readOctave rem) loc)
    else error "Badly formatted trace"

readLoc :: String -> [(Loc, String)]
readLoc = reads

readPitchClass :: String -> [(E.PitchClass, String)]
readPitchClass = reads

readOctave :: String -> [(E.Octave, String)]
readOctave = reads

readDuration :: String -> [(E.Dur, String)]
readDuration s = [(dur, rem)]
    where
        llex :: String -> [(String, String)]
        llex = lex
        [(s, rem)] = llex s
        dur = strToDur s

main = do
    [fname] <- getArgs
    file <- readFile fname
    let (prog, traces) = interp file
    run file
    putStrLn "\nSource:"
    putStrLn file
    putStrLn "Program:"
    putStrLn (show prog)
    putStrLn "\nTraces:"
    mapM_ (putStrLn . show) traces
    newTraces <- fmap changesToTraces promptChanges
    putStrLn (show newTraces)
    let newProg = updateProg prog traces newTraces
    let newP = unparse newProg
    run newP
    return ()
