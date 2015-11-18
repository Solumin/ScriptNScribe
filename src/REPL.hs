import BreveEval
import System.Environment

promptChanges :: IO [String]
promptChanges = putStrLn "Enter changes (blank line when done)" >> changes []

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
    putStrLn file
    run file
    changes <- fmap (map words) promptChanges
    mapM_ (putStrLn . unwords) changes
