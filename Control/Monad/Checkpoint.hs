-- | Providing \"checkpointed\" versions of common monad sequencing
-- actions (@'sequence'@, @'mapM'@, @'foldM'@ etc.), by serialising
-- remaining computation (and captured intermediate state) into a file
-- in-between executions.
--
-- This works for all monads @m@ which are instance of @'MonadIO'@,
-- i.e.  support embedding IO actions, as the necessary file IO
-- (saving and restoring checkpoints) must be @'lift'@ed from the IO
-- monad into the monad @m@.
--
-- Future versions of this module should use @'Traversable'@ data
-- types instead of committing to just lists.

module Control.Monad.Checkpoint
    ( -- * Basic checkpointing support
      checkpoint
    , recovering
      -- $DeletingFiles

      -- * Checkpointed monadic combinators
    , sequenceC, sequenceC_, replicateMC, replicateMC_ 
    , mapMC, mapMC_, forMC, forMC_
    , filterMC, zipWithMC, foldMC, foldMC_
    ) where

import GHC.Packing

import Data.Binary
import Data.Typeable
import System.Directory
import System.IO(openBinaryTempFile,hClose)

import Control.Monad.IO.Class -- to embed IO actions
import Control.Exception as E

-- | Checkpointing monadic actions which can embed IO: store the
-- monadic action to a file before running it. Conceptually, this is
-- 
-- @checkpoint name actions = liftIO (encodeToFile name actions) >> actions@
--
-- The code here makes file I/O atomic by using a tmp file and
-- renaming it to the desired name later.
checkpoint :: (Typeable a, Typeable m, MonadIO m) => FilePath -> m a -> m a
checkpoint name actions 
    = liftIO (do (path,hdl) <- openBinaryTempFile "." name
                 hClose hdl
                 encodeToFile path actions
                 renameFile path name) 
      >> actions

-- | Recovers and runs a monadic action from a checkpoint file if
-- possible, otherwise runs the supplied monadic action.
--
-- The type of the recovered monadic action is checked, but no
-- guarantees can be given about what it actually _does_, especially
-- whether it resembles the actions passed as the second argument.
recovering :: (Typeable a, Typeable m, MonadIO m) => FilePath -> m a -> m a
recovering name actions
    = (liftIO (decodeFromFile name 
                `catch` (\e -> print (e::SomeException) >> return actions)))
      >>= id
    -- = do haveFile <- liftIO (doesFileExist name)
    --      if not haveFile then actions
    --         else liftIO (putStrLn ("recovering from file " ++ name)) >> 
    --              liftIO (E.catch (decodeFromFile name) orActions) >>= id
    -- where orActions e = do putStrLn ("failed: " ++ show (e::PackException))
    --                        return actions

-- $DeletingFiles
-- Maybe some users would want to delete the checkpoint file after
-- completion of the entire action. This is easy to achieve for some
-- operations, harder for others; and again other users might want to
-- keep the checkpoints, to restart computations more than once.  As
-- this code is anyway fully explicit about the file naming, we leave
-- to the user to delete the checkpoint files after completion of the
-- operation if desired.


----------------------------------------

-- In the spirit of the @'Control.Monad'@ API, this module provides
-- sequence, replicateM, mapM, forM, filterM, zipWithM, foldM (and
-- underscored: sequence_ replicateM_ mapM_ forM_ zipWithM_ foldM_

-- | Run a list of action in sequence (from left to right),
-- checkpointing in between to a file with the name given as the first
-- argument, and return the results.
sequenceC :: (Typeable a, Typeable m, MonadIO m) => 
             FilePath -> [m a] -> m [a]
sequenceC  _   [] = return []
sequenceC name ms = recovering name (seqC_acc [] ms)
    where seqC_acc acc   []   = return (reverse acc) 
          seqC_acc acc (m:ms) = do x <- m
                                   checkpoint name $ 
                                     seqC_acc (x:acc) ms

-- | Run a list of action in sequence (from left to right),
-- checkpointing in between to a file with the name given as the first
-- argument. Return unit when done.
sequenceC_ :: (Typeable a, Typeable m, MonadIO m) => 
              FilePath -> [m a] -> m ()
-- sequenceC_ file ms = sequenceC file ms >> return ()
sequenceC_ file ms = recovering file (foldr (>>*) (return ()) ms)
    where m >>* m' = m >> checkpoint file m'

-- | Run the given action @m@ @n@ times, storing checkpoints to @file@
-- in between, return the list of results.
replicateMC :: (Typeable a, Typeable m, MonadIO m) => 
               FilePath -> Int -> m a -> m [a]
replicateMC file n m = sequenceC file (replicate n m)

-- | Run the given action @m@ @n@ times, storing checkpoints to @file@
-- in between, return unit when done.
replicateMC_ :: (Typeable a, Typeable m, MonadIO m) => 
                FilePath -> Int -> m a -> m ()
replicateMC_ file n m = sequenceC_ file (replicate n m)

-- | a @'mapM'@ with checkpoints. Equivalent to @sequenceC file . map f@
mapMC :: (Typeable b, Typeable m, MonadIO m) =>
         FilePath -> (a -> m b) -> [a] -> m [b]
mapMC file f xs  = sequenceC file (map f xs)

-- | a @'mapM_'@ with checkpoints. Equivalent to @sequenceC_ file . map f@
mapMC_ :: (Typeable b, Typeable m, MonadIO m) => 
          FilePath -> (a -> m b) -> [a] -> m ()
mapMC_ file f xs = sequenceC_ file (map f xs)

-- | Checkpointed monadic \"forall\" loop, which is @'mapMC'@ with
-- flipped arguments
forMC :: (Typeable b, Typeable m, MonadIO m) => 
         FilePath -> [a] -> (a -> m b) -> m [b]
forMC file = flip (mapMC file)

-- | @'mapMC_'@ with flipped arguments
forMC_ :: (Typeable b, Typeable m, MonadIO m) => 
         FilePath -> [a] -> (a -> m b) -> m ()
forMC_ file = flip (mapMC_ file)

-- | Generalises the @'filter'@ function to monadic actions, with
-- checkpoints.
--
-- The implementation uses mapMC for the checkpoint, capitalising on
-- the internal auto-recovery (the checkpoint is not useful outside
-- its context).
filterMC :: (Typeable a ,Typeable m, MonadIO m) => 
            FilePath -> (a -> m Bool) -> [a] -> m [a]
filterMC file pred xs = do flgs <- mapMC ("filterMC"++file) pred xs
                           return [ x | (x,True) <- zip xs flgs ]

-- | A monadic @zipWith@ with checkpoints. Defined using @'sequenceC'@.
zipWithMC :: (Typeable c ,Typeable m, MonadIO m) => 
             FilePath -> (a -> b -> m c) -> [a] -> [b] -> m [c]
zipWithMC file f xs ys = sequenceC file (zipWith f xs ys)

-- | Monadic left-fold (@'foldl'@).
--
-- Intermediate results are fed into
-- subsequent calls to the fold function when stepping from left to
-- right, checkpointing before running the function on one element.
foldMC :: (Typeable a ,Typeable m, MonadIO m) => 
          FilePath -> (a -> b -> m a) -> a -> [b] -> m a
foldMC _ _ e [] = return e
foldMC file f e xs = recovering file (fMC e xs)
    where fMC a   []   = return a
          fMC a (x:xs) = f a x >>= \fax ->checkpoint file (fMC fax xs)

-- | Like @'foldMC'@, but throws away the final result.
foldMC_ :: (Typeable a ,Typeable m, MonadIO m) => 
           FilePath -> (a -> b -> m a) -> a -> [b] -> m ()
foldMC_ file f e xs = foldMC file f e xs >> return ()
                      -- making our life simple...
