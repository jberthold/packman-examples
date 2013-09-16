-- providing "checkpointed" version of common monad sequencing actions
-- (sequence, mapM, foldM). This works for all monads which support
-- embedding IO actions (class MonadIO), which is necessary for the IO
-- action of saving the checkpoint to a file).

module CheckpointMonad2 where

import GHC.Packing

import Data.Binary
import Data.Typeable
import System.Directory
import System.IO(openBinaryTempFile,hClose)

import Control.Monad.IO.Class -- to embed IO actions
import Control.Exception as E

-- base
checkpoint :: (Typeable a, Typeable m, MonadIO m) => FilePath -> m a -> m a
-- checkpoint name actions = liftIO (encodeToFile name actions) >> actions
-- make it atomic, using a tmp file and moving it to the real name later
checkpoint name actions 
    = liftIO (do (path,hdl) <- openBinaryTempFile "." name
                 hClose hdl
                 encodeToFile path actions
                 renameFile path name) 
      >> actions

recovering :: (Typeable a, Typeable m, MonadIO m) => FilePath -> m a -> m a
recovering name actions 
    = do haveFile <- liftIO (doesFileExist name)
         if not haveFile then actions
            else liftIO (putStrLn ("recovering from file " ++ name)) >> 
                 liftIO (E.catch (decodeFromFile name) orActions) >>= id
    where orActions e = do putStrLn ("failed: " ++ show (e::PackException))
                           return actions
-- this allows for auto-recovering versions.  however, there is NO guarantee
-- that the file contains anything remotely resembling the actions passed as
-- the second argument...
-- A version without this auto-recovery (see other file) tends to seg-fault

-- We actually want the files to be deleted after completion of the entire
-- action. Easy to achieve for the recovering, harder for the ones below.

----------------------------------------

-- see Control.Monad API: we want to build (in this order) sequence,
-- replicateM, mapM, forM, filterM, zipWithM, foldM (and
-- underscored: sequence_ replicateM_ mapM_ forM_ zipWithM_ foldM_

-- implementations following the patterns in Control.Monad sometimes failed

-- evaluate each action from left to right, checkpointing in-between
sequenceC :: (Typeable a, Typeable m, MonadIO m) => FilePath -> [m a] -> m [a]
-- this tends to forget the bound x (is it only on the stack?):
-- sequenceC file ms = foldr k (return []) ms
--     where k m m' = m >>= \x -> checkpoint file (m' >>= \xs -> return (x:xs))
sequenceC  _   [] = return []
sequenceC name ms = recovering name (seqC_acc [] ms)
    where seqC_acc acc   []   = return (reverse acc) 
          seqC_acc acc (m:ms) = do x <- m
                                   checkpoint name $ 
                                     seqC_acc (x:acc) ms
-- this one keeps the results around

sequenceC_ :: (Typeable a, Typeable m, MonadIO m) => FilePath -> [m a] -> m ()
-- sequenceC_ file ms = sequenceC file ms >> return ()
sequenceC_ file ms = recovering file (foldr (>>*) (return ()) ms)
    where m >>* m' = m >> checkpoint file m'

replicateMC :: (Typeable a, Typeable m, MonadIO m) => Int -> FilePath -> m a -> m [a]
replicateMC n file m = sequenceC file (replicate n m)

replicateMC_ n file m = sequenceC_ file (replicate n m)

mapMC :: (Typeable b, Typeable m, MonadIO m) => FilePath -> (a -> m b) -> [a] -> m [b]
mapMC file f xs  = sequenceC file (map f xs)

mapMC_ :: (Typeable b, Typeable m, MonadIO m) => FilePath -> (a -> m b) -> [a] -> m ()
mapMC_ file f xs = sequenceC_ file (map f xs)

forM :: (Typeable b, Typeable m, MonadIO m) => FilePath -> [a] -> (a -> m b) -> m [b]
forM file = flip (mapMC file)

forM_ :: (Typeable b, Typeable m, MonadIO m) => FilePath -> [a] -> (a -> m b) -> m ()
forM_ file = flip (mapMC_ file)

filterMC :: (Typeable a ,Typeable m, MonadIO m) => FilePath -> (a -> m Bool) -> [a] -> m [a]
-- similar problem to sequenceC using foldr: only last flag is kept, rest gone.
-- filterMC _ _ [] = return []
-- filterMC file pred (x:xs) = pred x >>= \flg -> 
--                             checkpoint file (filterMC file pred xs >>= \ys ->
--                                              return (if flg then x:ys else ys))
-- using mapMC: this now works due to "auto-recovery" (checkpoint inside mapMC)
filterMC file pred xs = do flgs <- mapMC ("filterMC"++file) pred xs
                           return [ x | (x,True) <- zip xs flgs ]

zipWithMC :: (Typeable c ,Typeable m, MonadIO m) => 
             FilePath -> (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithMC file f xs ys = sequenceC file (zipWith f xs ys)

foldMC :: (Typeable a ,Typeable m, MonadIO m) => 
          FilePath -> (a -> b -> m a) -> a -> [b] -> m a
foldMC _ _ e [] = return e
foldMC file f e xs = recovering file (fMC e xs)
    where fMC a   []   = return a
          fMC a (x:xs) = f a x >>= \fax ->checkpoint file (fMC fax xs)

foldMC_ :: (Typeable a ,Typeable m, MonadIO m) => 
           FilePath -> (a -> b -> m a) -> a -> [b] -> m ()
foldMC_ file f e xs = foldMC file f e xs >> return () -- making our life simple...
