import System.IO
import System.Directory
import Data.List
import Control.Exception (bracketOnError)

todoFile :: String
todoFile = "todo.txt"

main = do
    contents <- readFile todoFile
    let todoTasks = lines contents
        numberedTasks = numberLines todoTasks
    putStrLn "These are your TO-DO items:"
    mapM_ putStrLn numberedTasks
    putStrLn "Which one do you want to delete?"
    numberString <- getLine
    let number = read numberString
        newTodoItems = unlines $ delete (todoTasks !! number) todoTasks
    replaceFile newTodoItems todoFile

numberLines :: [String] -> [String]
numberLines =
    zipWith
        (\n line ->
              show n ++ " - " ++ line)
        [0 ..]

replaceFile :: String -> FilePath -> IO ()
replaceFile newContents filepath =
    bracketOnError
        (openTempFile "." "temp")
        (\(tempName,tempHandle) -> do
             hClose tempHandle
             removeFile tempName)
        (\(tempName,tempHandle) -> do
             hPutStr tempHandle newContents
             hClose tempHandle
             removeFile filepath
             renameFile tempName filepath)