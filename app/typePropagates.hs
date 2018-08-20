
import GHC.Packing
import Data.Array.Unboxed
import Data.Word
import System.Environment

import Data.Typeable -- for analysis

import System.IO

-- this test exposes an overflow of Int in the list that is packed.  On 32-bit
-- platforms, 2^31 is enough to show it. On 64-bit platforms, Int is 8 bytes
-- (GHC choice), therefore we go higher.

main :: IO ()
main = do
        hSetBuffering stdout NoBuffering
        args <- getArgs

        putStrLn "Test program for packing/serialization:"
        putStrLn "testing how types propagate up, [Int]"
        let output = map (2^) ([30..33] ++ [62..66]) -- will be [Int] later
        putStrLn "evaluation of list `map (2^) ([30..33] ++ [62..66])'"
        
        if null args then putStrLn ("not touching output")
                     else putStrLn $ show $ take (read (head args)) output

        putStrLn "now packing the list (serialize)"
        packet1 <- trySerialize output
        putStrLn (show packet1)

        putStrLn "now unpacking (deserialize):"
        copy <- deserialize packet1 :: IO [Int] -- fixing the type!

        putStrLn ("unpacked, now evaluate")

        putStrLn (show copy)

        let copycast = map fromIntegral copy
        sequence_ [ putStrLn ("overflow computing " ++ show o 
                              ++ ", computed as " ++ show c)
                  | (o,c) <- zip output copycast, o /= c ]

        putStrLn "\n\nALL TESTS DONE"

