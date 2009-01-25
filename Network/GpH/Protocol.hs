-- | Parse a stream of requests and dump a stream of responses.
module Network.GpH.Protocol (decode, encode) where

import Data.ByteString
import Network.GpH.Types
import Data.Maybe (fromJust)
import Data.Bits
import Data.Word
import Control.Monad (forever)


newtype Dec a = Dec { unDec :: ByteString -> (a, ByteString) }

instance Monad Dec where
    return x = Dec $ \s -> (x, s)
    m >>= k  = Dec $ \s -> let (x, s') = unDec m s in unDec (k x) s'


-- Message codes, from 9p.h in Plan9 implementation of 9p.
msgCodes = Map.fromList $ zipWith (filter even [0..])
           [ version, auth, attach, undefined, flush, walk, open
           , create, read, write, clunk, remove, stat, wstat ]

decode :: ByteString -> [Request]
decode = fst . unDec (forever request)

encode :: [Reply] -> ByteString
encode = undefined

request :: Dec [Request]
request = do
  size <- word32
  req  <- byte
  tag  <- word16
  Map.lookup req msgCodes

-- Combinators

byte = Dec $ fromJust . uncons
bytes n = Dec $ \bs -> splitAt n bs

word16 :: Dec Word16
word16 = do x1 <- byte; x2 <- byte
            return $ shiftL (fromIntegral x1) 8 .|. fromIntegral x2

word32 :: Dec Word32
word32 = do x1 <- byte; x2 <- byte; x3 <- byte; x4 <- byte
            return $  shiftL (fromIntegral x1) 24
                  .|. shiftL (fromIntegral x2) 16
                  .|. shiftL (fromIntegral x3) 8
                  .|.         fromIntegral x4
