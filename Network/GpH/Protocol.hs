-- | Parse a stream of requests and dump a stream of responses.
module Network.GpH.Protocol (decode, encode) where

import qualified Data.ByteString as B
import Network.GpH.Types
import Data.Maybe (fromJust)
import Data.Bits
import Data.Word
import qualified Data.Map as Map
import Control.Monad
import Prelude hiding (read)


newtype Dec a = Dec { unDec :: B.ByteString -> (a, B.ByteString) }

instance Monad Dec where
    return x = Dec $ \s -> (x, s)
    m >>= k  = Dec $ \s -> let (x, s') = unDec m s in unDec (k x) s'


-- Message codes, from 9p.h in Plan9 implementation of 9p.
msgCodes = Map.fromList $ zip (filter even [0..])
           [ version, auth, attach, undefined, flush, walk, open
           , create, read, write, clunk, remove, stat, wstat ]

decode :: B.ByteString -> [Request]
decode = fst . unDec (forever request)

encode :: [Reply] -> B.ByteString
encode = undefined

request :: Dec Request
request = do
  size <- word32
  req  <- byte
  tag  <- word16
  msgCodes Map.! req

version = return Tversion `ap` arbitrary
auth    = return Tauth `ap` word32 `ap` arbitrary `ap` arbitrary
flush   = return Tflush `ap` word16
attach  = return Tattach `ap` word32 `ap` word32 `ap` arbitrary `ap` arbitrary
walk    = do f <- return Twalk `ap` word32 `ap` word32
             count <- word16
             return f `ap` replicateM (fromIntegral count) arbitrary
open    = return Topen `ap` word32 `ap` byte
create  = return Tcreate `ap` word32 `ap` arbitrary `ap` word32 `ap` byte
read    = return Tread `ap` word32 `ap` word64 `ap` word32
write   = do f <- return Twrite `ap` word32 `ap` word64
             count <- word32
             return f `ap` bytes (fromIntegral count)
clunk   = return Tclunk `ap` word32
remove  = return Tremove `ap` word32
stat    = return Tstat `ap` word32
wstat   = return Twstat `ap` word32 `ap` arbitrary

-- Combinators

byte = Dec $ fromJust . B.uncons
bytes n = Dec $ \bs -> B.splitAt n bs

-- | The number of bytes to parse is given in the first two bytes of the input.
arbitrary = word16 >>= \n -> Dec $ \bs -> B.splitAt (fromIntegral n) bs

word16 :: Dec Word16
word16 = do x1 <- byte; x2 <- byte
            return $ shiftL (fromIntegral x1) 8 .|. fromIntegral x2

word32 :: Dec Word32
word32 = do x1 <- word16; x2 <- word16
            return $ shiftL (fromIntegral x1) 16 .|. fromIntegral x2

word64 :: Dec Word64
word64 = do x1 <- word32; x2 <- word32
            return $ shiftL (fromIntegral x1) 32 .|. fromIntegral x2
