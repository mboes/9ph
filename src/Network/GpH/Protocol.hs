-- | Parse a stream of requests and dump a stream of responses.
module Network.GpH.Protocol (decode, encode) where

import Network.GpH.Protocol.Derive
import Network.GpH.Types
import Foreign.Storable
import Data.Binary hiding (decode, encode)
import Data.Word
import qualified Data.ByteString as B

instance Storable B.ByteString where
    alignment = undefined
    sizeOf = B.length

instance Storable [a] where
    alignment = undefined
    sizeOf = length

$(derive (undefined :: Request))

