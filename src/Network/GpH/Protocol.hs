-- | Parse a stream of requests and dump a stream of responses.
module Network.GpH.Protocol (decode, encode) where

import Network.GpH.Protocol.Derive
import Network.GpH.Types
import Data.Binary
import Data.Binary.Put
import Data.Binary.Get
import Data.Word
import Control.Monad (liftM)
import qualified Data.ByteString.Lazy.Char8 as B
import Data.BitSet (BitSet)
import qualified Data.BitSet as BitSet


instance Binary (BitSet QType) where
    get = do w :: Word8 <- get
             return $ BitSet.unsafeFromIntegral w
    put bs = put (BitSet.toIntegral bs :: Word8)

instance Binary (BitSet Permission) where
    get = do w :: Word32 <- get
             return $ BitSet.unsafeFromIntegral w
    put bs = put (BitSet.toIntegral bs :: Word32)

instance Binary (BitSet Mode) where
    get = do w :: Word8 <- get
             return $ BitSet.unsafeFromIntegral w
    put bs = put (BitSet.toIntegral bs :: Word8)

instance Binary (UField Info) where
    get = do len :: Word16 <- get
             liftM UF $ getLazyByteString (fromIntegral len)
    put (UF str) = do put (fromIntegral (B.length str) :: Word16)
                      putLazyByteString str

instance Binary (UField Name) where
    get = do len :: Word16 <- get
             liftM UF $ getLazyByteString (fromIntegral len)
    put (UF str) = do put (fromIntegral (B.length str) :: Word16)
                      putLazyByteString str

instance Binary (UField FData) where
    get = do len :: Word32 <- get
             liftM UF $ getLazyByteString (fromIntegral len)
    put (UF str) = do put (fromIntegral (B.length str) :: Word32)
                      putLazyByteString str

$(derive (undefined :: Qid))
$(derive (undefined :: Request))
$(derive (undefined :: Reply))
