import System.Random
import System.Environment
import System.IO

main = do
  gen <- getStdGen
  (filename:lenString:_) <- getArgs
  let len = 3 * read lenString
      paths = take len $ randomRs (1, 100) gen :: [Integer]
      contents = unlines $ map show paths
  pathFileHandle <- openFile filename WriteMode
  hPutStr pathFileHandle contents
  hClose pathFileHandle