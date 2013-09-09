{-# LANGUAGE Rank2Types #-}

-- care must be taken to not optimise away the memo effect. If any
-- optimisation is used, ghc will expand the function definition.

import GHC.Packing

import System.Environment
import System.CPUTime

import Data.MemoCombinators as Memo -- follow example from hackage

import System.Directory(doesFileExist)

-- for experimental code... 
import Data.Binary
import System.IO.Unsafe

-- memoized version of fibonacci
{-# NOINLINE fib1 #-}
-- fib1 :: Integer -> Integer
fib1 :: Integral a => a -> a
fib1 = memo fib1'
 where
  fib1' 0 = 1
  fib1' 1 = 1
  fib1' n = fib1 (n-1) + fib1 (n-2)
  memo = Memo.integral

-- we have to specialise on Integrals with this memo library

-- "unsafe global", requires monomorphic type
function :: Integer -> Integer
function = unsafePerformIO $ do
     haveFile <- doesFileExist filename
     if haveFile then putStrLn "Loading serialized function" >>
                      decodeFromFile filename
                 else let f = memo fib
                          memo = Memo.integral 
                          fib 0 = 1
                          fib 1 = 1
                          fib n = f (n-1) + f (n-2)
                      in putStrLn "no serialized function, start fresh" >>
                         return f

filename :: FilePath
filename = "fibmemocombinators2.serialized"

main = do args <- getArgs

          let nums = map read args :: [Integer] -- this fixes the type
              results = map function nums
          mapM_ timeEval results

          -- save function into a file
          encodeToFile filename function

          putStrLn "Done"

timeEval :: (Show a) => a -> IO ()
timeEval dat = do start <- getCPUTime
                  putStr (show dat)
                  stop <- getCPUTime
                  -- 3 480 217 000 000
                  let diff   = stop - start
                      diff_d = fromIntegral (diff `div` (10^6)) / (10^6)
                  putStrLn (" - in " ++ show diff_d ++ " sec")


