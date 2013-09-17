-- example program for serialisation: 
--   persistent memoisation by RTS-serialisation of a memoised function

import GHC.Packing

import System.Environment
import System.CPUTime
import Text.Printf

import Data.MemoCombinators as Memo

import System.Directory(doesFileExist)
import System.IO.Unsafe
import System.Mem

-- memoized version of fibonacci
{-# NOINLINE fib1 #-}
fib1 :: Integral a => a -> a
fib1 = memo fib1'
 where fib1' 0 = 0
       fib1' 1 = 1
       fib1' n = fib1 (n-1) + fib1 (n-2)
       memo = Memo.integral

-- we have to specialise on Integrals with this memo library

-- "unsafe global", requires monomorphic type
--function :: Integer -> Integer
{-# NOINLINE function #-}
function = unsafePerformIO $ do
     haveFile <- doesFileExist filename
     if haveFile then putStrLn "Loading serialized function" >>
                      decodeFromFile filename
                 else let {-# NOINLINE f #-} -- important!
                          f = memo fib
                          memo = Memo.integral 
                          fib 0 = 0::Integer -- needs fixed type
                          fib 1 = 1
                          fib n = f (n-1) + f (n-2)
                      in putStrLn "no serialized function, start fresh" >>
                         return f
                         
filename :: FilePath
filename = "fibmemocombinators2.serialized"

main = do args <- getArgs

          putStrLn $ "Calling a memoised fibonacci function "
                       ++ "on all given arg.s (read as Integer),\n " 
                       ++ "and then saving the memoised function"

          let nums = map read args :: [Integer] -- this fixes the type
              results = map function nums
          mapM_ timeEval results

          -- save function into a file
          function `seq` encodeToFile filename function
          -- we need to force the function here - the program
          -- would otherwise <<loop>> when saving it unevaluated

          putStrLn "Done"

timeEval :: (Show a) => a -> IO ()
timeEval dat = do start <- getCPUTime
                  printf "computed a %d digit number" (length (show dat))
                  stop <- getCPUTime
                  -- 3 480 217 000 000
                  let diff   = stop - start
                      diff_d = fromIntegral (diff) / (10^12) :: Double
                  printf " - in %.6f sec.\n" diff_d


