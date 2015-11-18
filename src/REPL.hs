import BreveEval
import System.Environment

promptChanges :: IO [String]
promptChanges = putStrLn ">>Enter changes: (line, col) val (blank line when done)" >> changes []

changes :: [String] -> IO [String]
changes ts = do
    putStr "> "
    s <- getLine
    case s of
        [] -> return ts
        t -> changes (t:ts)

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
    changes <- fmap (map words) promptChanges
    mapM_ (putStrLn . unwords) changes
