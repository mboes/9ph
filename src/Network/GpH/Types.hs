module Network.GpH.Types where

import Data.Word
import Data.ByteString.Lazy.Char8
import Data.Typeable
import Data.Data
import Data.BitSet (BitSet)


data QType = Qdir | Qappend | Qexcl | Qmount | Qauth | Qtmp
             deriving (Eq, Enum, Ord, Show, Data, Typeable)

data Permission = Pdir | Pappend | Pexcl | Pmount | Pauth | Ptmp
                  deriving (Eq, Enum, Ord, Show, Data, Typeable)

-- | A 13-byte wide field used to represent server-side unique identifiers.
data Qid = Qid { qid_type    :: BitSet QType
               , qid_version :: Word32
               , qid_path    :: Word64 }
    deriving (Eq, Ord, Show, Data, Typeable)

-- | For version, error messages.
data Info deriving Typeable

-- | Used to hold user names, file names.
data Name deriving Typeable

-- | An arbitrary size field containing data read / to write.
data FData deriving Typeable

-- | A field of arbitrary size. The type constructor is indexed by a
-- phantom type indicating the semantics of the field and how to
-- decode/encode it.
newtype UField a = UF ByteString
    deriving (Eq, Ord, Show, Data, Typeable)

type Size = Word32
type Tag = Word16
type Offset = Word64
type Fid = Word32
type Mode = Word8
type Count = Word32
type IOUnit = Word32

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

data Request = Tversion Size (UField Info)
             | Tauth Word32 (UField Name) (UField Name)
             | Tflush Tag
             | Tattach Fid Fid (UField Name) (UField Name)
             | Twalk Fid Fid [UField Name]
             | Topen Fid Mode
             | Tcreate Fid (UField Name) (BitSet Permission) Mode
             | Tread Fid Offset Count
             | Twrite Fid Offset (UField FData)
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

data Reply = Rversion Size (UField Info)
           | Rauth Qid
           | Rerror (UField Info)
           | Rflush
           | Rattach Qid
           | Rwalk [Qid]
           | Ropen Qid IOUnit
           | Rcreate Qid IOUnit
           | Rread (UField FData)
           | Rwrite Count
           | Rclunk
           | Rremove
           | Rstat ByteString
           | Rwstat
             deriving (Eq, Ord, Show, Data, Typeable)
