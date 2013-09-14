
import CheckpointMonad2
import System.Environment
import Control.Concurrent
import System.Directory

main = do args <- getArgs
          putStrLn "Checkpointed monadic action, hit ^C and re-run any time"
          let n = if null args then 10 else read (head args)::Int
          (mapMC "mapMC_test2" doIt [1..n] >>= print)
          (foldMC "foldMC_test2" foldIt [] [1..n] >>= print)
          (mapMC_ "mapMC__test2" forgetIt [1..n]  >>= print)
          (filterMC "filMC_test2" (filterIt "file") [1..n]  >>= print)

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


