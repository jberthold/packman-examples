-- lazyIO: This is the first line.
import GHC.Packing

import Control.DeepSeq
import Control.Monad
import System.Directory
import System.Environment
import System.FilePath
import System.IO


usage = unlines $
        "This program reads its source code using lazy IO (readFile)":
        "and tries to serialize the read text. This fails when not":
        "forcing the entire text before serialisation, as the file":
        "is only semi-closed and the file handle contains an MVar.":
        []

main = do putStrLn usage
          name <- getProgName
          args <- getArgs

          let file = "app" </> name <> ".hs"

          txt  <- readFile file
          putStrLn (head (lines txt)) -- No
          when (not (null args)) $ putStrLn (last (lines txt)) -- Yes
          blob <- trySerialize txt -- (force txt)
          putStrLn (take 1000 (show blob) <> "\n...")

-- BTW forcing it like this does _not_ help, will just pack the
-- "force" as well.
main2 = print =<< trySerialize . force =<< readFile . ("app" </>) . (<> ".hs") =<< getProgName

-- lazyIO: This is the last line.
