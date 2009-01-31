module Network.GpH.Types where

import Data.Word
import Data.ByteString.Lazy.Char8
import Data.Typeable
import Data.Data


type Size = Word32
type Tag = Word16
type Offset = Word64
type Field = ByteString
type Fid = Word32
type Mode = Word8
type Count = Word32
type IOUnit = Word32
type Permission = Word32

-- | A 13-byte wide field used to represent server-side unique identifiers.
newtype Qid = Qid ByteString

-- | An arbitrary size field containing data read / to write. Can be
-- larger than regular Names (whose max size is 2^16 bytes).
newtype Data = Data ByteString

-- Based on the following abstraction of the wire protocol for
-- requests given in the Plan9 Fourth Edition manual:
--
--    Tversion msize[4] version[s]
--    Tauth afid[4] uname[s] aname[s]
--    Tflush oldtag[2]
--    Tattach fid[4] afid[4] uname[s] aname[s]
--    Twalk fid[4] newfid[4] nwname[2] nwname*(wname[s])
--    Topen fid[4] mode[1]
--    Tcreate fid[4] name[s] perm[4] mode[1]
--    Tread fid[4] offset[8] count[4]
--    Twrite fid[4] offset[8] count[4] data[count]
--    Tclunk fid[4]
--    Tremove fid[4]
--    Tstat fid[4]
--    Twstat fid[4] stat[n]

data Request = Tversion Size ByteString
             | Tauth Word32 ByteString ByteString
             | Tflush Tag
             | Tattach Fid Fid ByteString ByteString
             | Twalk Fid Fid [ByteString]
             | Topen Fid Mode
             | Tcreate Fid ByteString Permission Mode
             | Tread Fid Offset Count
             | Twrite Fid Offset Data
             | Tclunk Fid
             | Tremove Fid
             | Tstat Fid
             | Twstat Fid ByteString
               deriving (Eq, Ord, Show, Data, Typeable)

-- Based on the following abstraction of the wire protocol for
-- responses given in the Plan9 Fourth Edition manual:
--
--    Rversion msize[4] version[s]
--    Rauth aqid[13]
--    Rerror ename[s]
--    Rflush
--    Rattach qid[13]
--    Rwalk nwqid[2] nwqid*(wqid[13])
--    Ropen qid[13] iounit[4]
--    Rcreate qid[13] iounit[4]
--    Rread count[4] data[count]
--    Rwrite count[4]
--    Rclunk
--    Rremove
--    Rstat stat[n]
--    Rwstat

data Reply = Rversion Size ByteString
           | Rauth Qid
           | Rerror ByteString
           | Rflush
           | Rattach Qid
           | Rwalk [Qid]
           | Ropen Qid IOUnit
           | Rcreate Qid IOUnit
           | Rread Data
           | Rwrite Count
           | Rclunk
           | Rremove
           | Rstat ByteString
           | Rwstat
             deriving (Eq, Ord, Show, Data, Typeable)
