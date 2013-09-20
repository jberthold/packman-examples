import GHC.Packing

import System.IO
import System.Environment

import Control.DeepSeq


usage = unlines $
        "This program reads its source code using lazy IO (readFile)":
        "and tries to serialize the read text. This fails when not":
        "forcing the entire text before serialisation, as the file":
        "is only semi-closed and the file handle contains an MVar.":
        []

-- BTW forcing it like this does _not_ help, will just pack the
-- "force" as well.
-- main = print =<< trySerialize . force =<< readFile . (++ ".hs") =<< getProgName

main = do putStrLn usage
          name <- getProgName
          txt  <- readFile (name ++ ".hs")
          putStrLn (head (lines txt)) -- No
          -- putStrLn (last (lines txt)) -- Yes
          blob <- trySerialize txt
          putStrLn (show blob)


