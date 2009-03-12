module Network.GpH.Types where

import Data.Word
import Data.ByteString.Lazy.Char8
import Data.Typeable
import Data.Data
import Data.BitSet (BitSet)


-- | Type of a qid.
data QType = Qdir | Qappend | Qexcl | Qmount | Qauth | Qtmp
             deriving (Eq, Ord, Show, Data, Typeable)

data Permission = Pdir | Pappend | Pexcl | Pmount | Pauth | Ptmp
                  deriving (Eq, Ord, Show, Data, Typeable)

-- | Open mode.
data Mode = Oread | Owrite | Ordwr | Oexec | Otrunc | Orexec | Orclose | Oappend | Oexcl
                  deriving (Eq, Ord, Show, Data, Typeable)

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
             | Topen Fid (BitSet Mode)
             | Tcreate Fid (UField Name) (BitSet Permission) (BitSet Mode)
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

-- Bits set by each flag, adapted from 9p.h.

instance Enum QType where
    fromEnum Qdir    = 0x80000000
    fromEnum Qappend = 0x40000000
    fromEnum Qexcl   = 0x20000000
    fromEnum Qmount  = 0x10000000
    fromEnum Qauth   = 0x08000000
    fromEnum Qtmp    = 0x04000000

    toEnum 0x80000000 = Qdir
    toEnum 0x40000000 = Qappend
    toEnum 0x20000000 = Qexcl
    toEnum 0x10000000 = Qmount
    toEnum 0x08000000 = Qauth
    toEnum 0x04000000 = Qtmp

instance Enum Permission where
    fromEnum Pdir    = 0x80000000
    fromEnum Pappend = 0x40000000
    fromEnum Pexcl   = 0x20000000
    fromEnum Pmount  = 0x10000000
    fromEnum Pauth   = 0x08000000
    fromEnum Ptmp    = 0x04000000

    toEnum 0x80000000 = Pdir
    toEnum 0x40000000 = Pappend
    toEnum 0x20000000 = Pexcl
    toEnum 0x10000000 = Pmount
    toEnum 0x08000000 = Pauth
    toEnum 0x04000000 = Ptmp

instance Enum Mode where
    fromEnum Oread = 0x00
    fromEnum Owrite = 0x01
    fromEnum Ordwr = 0x02
    fromEnum Oexec = 0x03
    fromEnum Otrunc = 0x10
    fromEnum Orexec = 0x20
    fromEnum Orclose = 0x40
    fromEnum Oappend = 0x80
    fromEnum Oexcl = 0x1000

    toEnum 0x00 = Oread
    toEnum 0x01 = Owrite
    toEnum 0x02 = Ordwr
    toEnum 0x03 = Oexec
    toEnum 0x10 = Otrunc
    toEnum 0x20 = Orexec
    toEnum 0x40 = Orclose
    toEnum 0x80 = Oappend
    toEnum 0x1000 = Oexcl
