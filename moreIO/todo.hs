import System.Environment
import Control.Exception (bracketOnError)
import System.Directory
import System.IO
import Data.List
import System.Environment

main = do
    args <- getArgs
    let (command,argList) = parseArgs args
    dispatch command argList

parseArgs :: [String] -> (Maybe String, Maybe [String])
parseArgs [] = (Nothing, Nothing)
parseArgs (x:xs) = (Just x, Just xs)

dispatch :: Maybe String -> Maybe [String] -> IO ()
dispatch Nothing = noArgs
dispatch (Just "add") = add
dispatch (Just "view") = view
dispatch (Just "remove") = remove
dispatch (Just command) = doesNotExist command

noArgs :: Maybe [String] -> IO ()
noArgs _ = putStrLn "No command supplied"

doesNotExist :: String -> Maybe [String] -> IO ()
doesNotExist command _ =
    putStrLn $ "The " ++ command ++ " command doesn't exist"

add :: Maybe [String] -> IO ()
add (Just [filename,todoItem]) = appendFile filename $ todoItem ++ "\n"
add _ = arityMessage "add" 2

arityMessage :: String -> Int -> IO ()
arityMessage command arity =
    putStrLn $
    "The " ++
    command ++ " command takes exactly " ++ show arity ++ " arguments"

view :: Maybe [String] -> IO ()
view (Just [filename]) = do
    contents <- readFile filename
    let numberedTasks = numberLines $ lines contents
    putStr $ unlines numberedTasks
view _ = arityMessage "view" 1

numberLines :: [String] -> [String]
numberLines = zipWith addLineNumber [0 ..]
  where
    addLineNumber n line = show n ++ " - " ++ line

remove :: Maybe [String] -> IO ()
remove (Just [filename,numberString]) = do
    contents <- readFile filename
    let index = read numberString
        tasks = lines contents
        newTodoItems = unlines $ removeLine (get index tasks) tasks
    replaceFile newTodoItems filename
remove _ = arityMessage "remove" 2

removeLine :: Maybe String -> [String] -> [String]
removeLine Nothing xs = xs
removeLine (Just s) xs = delete s xs

get :: Int -> [a] -> Maybe a
get _ [] = Nothing
get i (x:xs) =
    if i == 0
        then Just x
        else get (i - 1) xs

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