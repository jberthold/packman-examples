{-# LANGUAGE CPP #-}

import CheckpointMonad
import System.Environment
import Control.Concurrent

import System.Directory

-- import Foreign.Storable
-- import Foreign.C
-- import GHC.Ptr

-- -- Well, a hack! Does not help, though.
-- -- we can get debug messages with -DG
-- foreign import ccall "&keepCAFs" keepCAFs :: Ptr CInt
-- setKeepCAFs = poke keepCAFs 1

main = do args <- getArgs
          let doRecover = not (null args) && head args == "-r"
              -- or we read n, the number of repetitions 
              n = if doRecover || null args 
                  then 10 else read (head args)::Int
          putStrLn ("Size is " ++ show n) 
          -- xs <- if doRecover then recover "seqC_test"
          --                    else mapMC "seqC_test" doIt [1..n]
          -- xs <- if doRecover then recover "foldMC_test"
          --                    else foldMC "foldMC_test" foldIt [] [1..n]
          -- xs <- if doRecover then recover "seqC_test"
          --                    else mapMC_ "seqC_test" forgetIt [1..n]
          xs <- if doRecover then recover "filMC_test"
                             else filterMC "filMC_test" (filterIt "file") [1..n]
          putStrLn (show xs)
doIt x = do putStrLn ("start for action " ++ show x)
            threadDelay 300000
            putStrLn "returning something"
            return x

foldIt xs x = do putStr ("folding in " ++ show x)
                 threadDelay 300000
                 putStrLn ("..now folding it")
                 return (x:xs)

forgetIt x = do putStr ("fire and forget: " ++ show x)
                threadDelay 300000
                putStrLn ("..OK, next please")

filterIt prefix x = do let name = prefix ++ show x
                       putStrLn ("File " ++ name)
                       -- doesFileExist name -- fails at runtime (MUT_VAR_DIRTY)
                       threadDelay 200000
                       ex <- doesFileExist name
                       if ex then (putStrLn " exists!") else putStrLn " not found"
                       return ex


