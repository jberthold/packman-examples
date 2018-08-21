module Main where

import Data.MemoCombinators
import GHC.Packing
import System.IO.Unsafe
import System.Directory
import System.Environment
import System.CPUTime
import Text.Printf

import Data.Binary
import Data.Typeable

-- "Ackermann"
ack :: Integer -> Integer -> Integer
ack = memo2 integral integral ack'
    where ack' 0 m = m+1
          ack' n 0 = ack (n-1) 1
          ack' n m = ack (n-1) (ack n (m-1))

-- to memoise, use two stages (see Memocombinators manual), and wrap into
-- serialisation as a CAF
ack_memo = unsafePerformIO $
           do haveFile <- doesFileExist ackFile
              if haveFile then decodeFromFile ackFile
                          else -- return ack
                              let ac = memo2 integral integral ac0
                                  ac0 0 m = m+1
                                  ac0 n 0 = ac (n-1) 1
                                  ac0 n m = ac (n-1) (ac n (m-1))
                              in return ac
ackFile = "ack_memo.serialised"
bigBuffer = 64000000

main = do args <- getArgs
          putStrLn $ "Calling a memoised Ackermann function "
                       ++ "on all given pairs of arg.s (read as Integer),\n " 
                       ++ "and then saving the memoised function"

          let nums = map read args :: [Integer] -- this fixes the type
              results = [ ack_memo a b | (a,b) <- pairUp nums]
              pairUp [] = []
              pairUp (x:y:xs) = (x,y):pairUp xs
              pairUp (x:[]) = [(x,x)]
          mapM_ timeEval results

          -- save function into a file
          ack_memo `seq` encodeToFileWith bigBuffer ackFile ack_memo
          -- we need to force the function here - the program
          -- would otherwise <<loop>> when saving it unevaluated

          putStrLn "Done"

encodeToFileWith :: Typeable a => Int -> FilePath -> a -> IO ()
encodeToFileWith sz path x = trySerializeWith x sz >>= encodeFile path

timeEval :: (Show a) => a -> IO ()
timeEval dat = do start <- getCPUTime
                  printf "computed a %d digit number" (length (show dat))
                  stop <- getCPUTime
                  -- 3 480 217 000 000
                  let diff   = stop - start
                      diff_d = fromIntegral (diff) / (10^12) :: Double
                  printf " - in %.6f sec.\n" diff_d

