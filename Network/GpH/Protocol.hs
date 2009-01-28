-- | Parse a stream of requests and dump a stream of responses.
module Network.GpH.Protocol (decode, encode) where

import qualified Data.ByteString.Lazy as B
import Network.GpH.Types
import Control.Monad
import Data.Word
import Data.Array.IArray
import Data.Data
import Data.Generics.Aliases
import Data.Binary
import Prelude hiding (read)


newtype Dec a = Dec { unDec :: B.ByteString -> (a, B.ByteString) }

instance Monad Dec where
    return x = Dec $ \s -> (x, s)
    m >>= k  = Dec $ \s -> let (x, s') = unDec m s in unDec (k x) s'

decode :: B.ByteString -> [Request]
decode = fst . unDec (forever request)

reply :: Put
reply = undefined

request :: Get Request
request = do
  size <- word32
  req  <- getWord8
  tag  <- word32
  fromConstrM dispatch (lookupConstr req)
    where dispatch :: Data d => Get d
          dispatch = error "Impossible"
                     `extR` word16 `extR` word32 `extR` word64
                     `extR` arbitrary `extR` list arbitrary

lookupConstr = let constructors :: Array Int Constr
                   constructors =
                       listArray (0, 14) $ dataTypeConstrs
                                     $ dataTypeOf (undefined :: Request)
               in \i -> constructors ! ((fromIntegral i - 100) `div` 2)

-- Combinators

-- | The number of bytes to parse is given in the first two bytes of the input.
arbitrary = word16 >>= \n -> replicateM (fromIntegral n) getWord8
list m = word16 >>= \n -> replicateM (fromIntegral n) m

word16 = get :: Get Word16
word32 = get :: Get Word32
word64 = get :: Get Word64
