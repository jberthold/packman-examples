
import GHC.Packing

import qualified Data.Array.IArray as A
import Control.Concurrent

import System.Environment
import System.IO
import qualified Control.Exception as E

import Control.Parallel.Eden.ParPrim

-- this test uses the trySerialize routine. We expect to trigger some
-- exceptions and catch them as appropriate.

catchPackExc :: IO () -> IO ()
catchPackExc io = io `E.catch` (\e -> putStrLn (show (e::PackException)))

main :: IO ()
main=do hSetBuffering stdout NoBuffering
        args <- getArgs

        putStrLn "Test program for packing/serialization:"
        putStrLn "testing exceptions during packing. Use -qQ1k or so..."

        let n    = if (length args < 2) then 1 else read (args!!1)
            size = if null args then 128 else read (head args)::Int
            arr :: A.Array Int Int
            arr  = A.array (0,size-1) 
                   [ (i,i) | i <- [0..size-1] ]

        let output = A.amap (2*) arr
        putStrLn $ show $ take n $ A.elems output

        putStrLn "now packing the array (buffer big enough?)"
        
        catchPackExc $
         do packet1 <- trySerialize output
            putStrLn (show packet1)
            putStrLn "now unpacking (deserialize):"
            copy <- deserialize packet1

            putStrLn ("unpacked, now evaluate")
            putStrLn (show copy)

        putStrLn "packing some forbidden types"
        t <- myThreadId
        putStrLn "next should be unsupported"
        catchPackExc (trySerialize t >>= print)

        m <- newEmptyMVar :: IO (MVar Int)
        putStrLn "next should be cannotpack"
        catchPackExc (trySerialize m >>= print)

        (c,b) <- createC:: IO (ChanName' Double, Double)
        putStrLn "next should hit a blackhole"
        catchPackExc (trySerialize b >>= print)
        
        let arr2 = A.listArray (0,n-1) (take n (A.elems arr)) :: A.Array Int Int
        putStrLn "this - finally - should work"
        putStrLn ( show $ arr2 A.! 0 ) -- forcing it
        catchPackExc $
          do p2 <- trySerialize arr2
             arr3 <- deserialize p2
             print arr3
        putStrLn "DONE"
