-- | Parse a stream of requests and dump a stream of responses.
module Network.GpH.Protocol (decode, encode) where

import qualified Data.ByteString as B
import Network.GpH.Types
import Data.Maybe (fromJust)
import Data.Bits
import Data.Word
import Data.Array.IArray
import Data.Data
import Data.Generics.Aliases
import Control.Monad
import Prelude hiding (read)


newtype Dec a = Dec { unDec :: B.ByteString -> (a, B.ByteString) }

instance Monad Dec where
    return x = Dec $ \s -> (x, s)
    m >>= k  = Dec $ \s -> let (x, s') = unDec m s in unDec (k x) s'

decode :: B.ByteString -> [Request]
decode = fst . unDec (forever request)

encode :: [Reply] -> B.ByteString
encode = undefined

request :: Dec Request
request = do
  size <- word32
  req  <- byte
  tag  <- word32
  fromConstrM dispatch (lookupConstr req)
    where dispatch :: Data d => Dec d
          dispatch = error "Impossible."
                     `extR` word16 `extR` word32 `extR` word64
                     `extR` arbitrary `extR` list arbitrary

lookupConstr = let constructors :: Array Int Constr
                   constructors =
                       listArray (0, 14) $ dataTypeConstrs
                                     $ dataTypeOf (undefined :: Request)
               in \i -> constructors ! ((fromIntegral i - 100) `div` 2)

-- Combinators

byte = Dec $ fromJust . B.uncons
bytes n = Dec $ \bs -> B.splitAt n bs

-- | The number of bytes to parse is given in the first two bytes of the input.
arbitrary = word16 >>= \n -> Dec $ \bs -> B.splitAt (fromIntegral n) bs

list m = word16 >>= \n -> replicateM (fromIntegral n) m

word16 :: Dec Word16
word16 = do x1 <- byte; x2 <- byte
            return $ shiftL (fromIntegral x1) 8 .|. fromIntegral x2

word32 :: Dec Word32
word32 = do x1 <- word16; x2 <- word16
            return $ shiftL (fromIntegral x1) 16 .|. fromIntegral x2

word64 :: Dec Word64
word64 = do x1 <- word32; x2 <- word32
            return $ shiftL (fromIntegral x1) 32 .|. fromIntegral x2
