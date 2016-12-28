import System.IO
       (withFile, Handle, IOMode(ReadMode), hGetContents, hClose,
        openFile)
import Control.Exception (bracket)

main = do
    withFile''
        "girlfriend.txt"
        ReadMode
        (\handle -> do
             contents <- hGetContents handle
             putStr contents)

withFile' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile' name mode f =
    bracket
        (openFile name mode)
        (\handle ->
              hClose handle)
        (\handle ->
              f handle)

withFile'' :: FilePath -> IOMode -> (Handle -> IO a) -> IO a
withFile'' name mode f = bracket (openFile name mode) hClose f