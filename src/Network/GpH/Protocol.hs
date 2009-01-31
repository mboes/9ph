-- | Parse a stream of requests and dump a stream of responses.
module Network.GpH.Protocol (decode, encode) where

import Network.GpH.Protocol.Derive
import Network.GpH.Types
import Data.Binary hiding (decode, encode)
import Data.Binary.Put
import Data.Binary.Get
import Data.Word
import qualified Data.ByteString.Lazy.Char8 as B


class Protocol a where
    size   :: a -> Word32
    decode :: B.ByteString -> a
    encode :: a -> B.ByteString

$(derive (undefined :: Request))

$(derive (undefined :: Reply))
