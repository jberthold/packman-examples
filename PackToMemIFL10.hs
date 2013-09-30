{-# OPTIONS -XScopedTypeVariables -XRecordWildCards -XMagicHash -XUnboxedTuples -XBangPatterns #-}

module PackToMem
    where 

import GHC.IO ( IO(..) )
import GHC.Prim
    ( ByteArray#, sizeofByteArray#, serialize#, deserialize# )
import GHC.Exts ( Int(..), Word )
import Data.Array.Base ( UArray(..), elems, listArray )
import Foreign.Storable ( sizeOf )

-- Read and Show instances
import Text.Printf ( printf )
import Text.ParserCombinators.ReadP (sepBy1, many1, ReadP, munch,
    munch1, pfail, readP_to_S, satisfy, skipSpaces, string )
import Data.Char ( isDigit )
import Data.Typeable ( Typeable(..), TypeRep, typeOf ) 
              -- for dynamic type checks when parsing

import Data.Binary ( Get, Binary(..), encode, decode, encodeFile, decodeFile )

----------------------------------------------------------
-- IO wrappers for primitive operation:
--
---------

heapToArray :: a -> IO (UArray Int Word)
heapToArray x 
    = IO (\s -> 
           case serialize# x s of 
             (# s', bArr# #) -> 
                   case sizeofByteArray# bArr#  of 
                      sz# -> let -- we return a word array, need to
                                 -- adjust size. OTOH, the ByteArray
                                 -- size is always multiple of
                                 -- sizeOf(StgWord) by construction.
                                 size  = (I# sz# ) `div` 
                                            sizeOf(undefined::Word) 
                                 upper = size - 1
                             in (# s', UArray 0 upper size bArr#  #) )


heapFromArray :: UArray Int Word -> IO a
heapFromArray (UArray 0 hi size bArr# ) 
    = IO (\s -> case deserialize# bArr# s of
                  (# s', newData #) -> (# s', newData #))
heapFromArray other 
    = error "internal error: unexpected array bounds"

-- Type-safe variant

-- phantom type a ensures that we do not unpack rubbish
-- size is needed for parsing packets
data Serialized a = Serialized { packetData :: ByteArray# } 

serialize :: a -> IO (Serialized a)
serialize x 
    = IO (\s -> 
           case serialize# x s of 
             (# s', bArr# #) -> (# s', Serialized { packetData=bArr# } #) )

deseri_ :: ByteArray# -> IO a
deseri_ bArr# = IO (\s -> case deserialize# bArr# s of
                              (# s', newData #) -> (# s', newData #))


deserialize :: Serialized a -> IO a
deserialize ( Serialized{..} ) = do deseri_ packetData

-----------------------------------------------
-- Show Instance: 
--    print packet as Word array in 4 columns
-----------------------------------------------

instance Typeable a => Show (Serialized a) where
    show (Serialized {..} ) 
        = "Serialization Packet, size " ++ show size 
          ++ ", type " ++ show t ++ "\n"
          ++ showWArray (UArray 0 (size-1) size packetData )
        where size = case sizeofByteArray# packetData of 
                          sz# -> (I# sz# ) `div` sizeOf(undefined::Word)
              t    = typeOf ( undefined :: a )

-- show a serialized structure as a packet (Word Array)
showWArray :: UArray Int Word -> String
showWArray arr = unlines [ show i ++ ":" ++ unwords (map showH row) 
                         | (i,row) <- zip  [0,4..] elRows ]
    where showH w = -- "\t0x" ++ showHex w " "
                    printf "\t0x%08x " w
          elRows = takeEach 4 (elems arr)

takeEach :: Int -> [a] -> [[a]]
takeEach n [] = [] 
takeEach n xs = first:takeEach n rest 
    where (first,rest) = splitAt n xs


-----------------------------------------------
-- Read Instance, using ReadP parser (base-4.2)
--  eats the format we output above (Show), but
--  can also consume other formats of the array 
--  (not implemented yet).
-----------------------------------------------

instance Typeable a => Read (Serialized a) 
    where readsPrec _ input 
           = case parseP input of
              []    -> error "No parse for packet"
              [((sz,tp,dat),r)] 
                    -> let !(UArray _ _ _ arr# ) = listArray (0,sz-1) dat
                           t = typeOf (undefined::a)
                       in if show t == tp 
                              then [(Serialized arr# , r)]
                              else error ("type error during packet parse: "
                                          ++ show t ++ " vs. " ++ tp)

              other -> error ("Ambiguous parse for packet: " ++ show other)


-- Parser: read header with size and type, then iterate over array
-- values, reading several hex words in one row, separated by tab and
-- space. Size is needed here to avoid returning prefixes.
parseP :: ReadS (Int, String,[Word])
parseP = readP_to_S $
         do string "Serialization Packet, size "
            sz_str <- munch1 isDigit
            let sz = read sz_str::Int
            string ", type "
            tp <- munch1 (not . (== '\n'))
            newline
            let startRow = do { many1 digit; colon; tabSpace }
                row = do { startRow; sepBy1 hexNum tabSpace }
            valss <- sepBy1 row newline
            skipSpaces -- eat remaining spaces
            let vals = concat valss
                l    = length vals
            -- filter out wrong lengths:
            if (sz /= length vals) then pfail
                                   else return (sz, tp, vals)

digit = satisfy isDigit
colon = satisfy (==':')
tabSpace = munch1 ( \x -> x `elem` " \t" )
newline = munch1 (\x -> x `elem` " \n")

hexNum :: ReadP Word -- we are fixing the type to what we need
hexNum = do string "0x"
            ds <- munch hexDigit
            return (read ("0x" ++ ds))
  where hexDigit = (\x -> x `elem` "0123456789abcdefABCDEF")

------------------------------------------------------------------
-- Binary instance, and some convenience wrappers

-- we make our life simple and construct/deconstruct Word (U)Arrays,
-- quite as we did in the Show/Read instances above already. The
-- TypeRep, however, has to be shown to a string to serialize.
instance Typeable a => Binary (Serialized a) where
    -- put :: (Serialized a) -> Put
    put (Serialized bArr#) 
        = do let typeStr = show (typeOf (undefined :: a))
                 arr     = UArray 0 (sz-1) sz bArr# :: UArray Int Word
                 sz      = case sizeofByteArray# bArr# of 
                             sz# -> (I# sz# ) `div` sizeOf(undefined::Word)
             put typeStr
             put arr
    get = do typeStr <- get :: Get String
             uarr    <- get :: Get (UArray Int Word)
             let !(UArray _ _ sz bArr#) = uarr
                 tp = typeOf (undefined :: a) -- for type check
             if (show tp == typeStr)
                   then return ( Serialized bArr# )
                   else error ("Type error during packet parse:\n\tExpected "
                               ++ show tp ++ ", found " ++ typeStr ++ ".")

encodeToFile :: Typeable a => FilePath -> a -> IO ()
encodeToFile path x = serialize x >>= encodeFile path

decodeFromFile :: Typeable a => FilePath -> IO a
decodeFromFile path = decodeFile path >>= deserialize
