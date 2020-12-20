module Core.Index where

-- Note: this index parser assumes index version 2

import Util.Util

import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.Binary                as Bin
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as L

import Data.Char
import Data.Int
import Data.List

data IndexEntry = IndexEntry
  { ctime :: B.ByteString
  , mtime :: B.ByteString
  , dev   :: B.ByteString
  , ino   :: B.ByteString
  , mode  :: B.ByteString
  , uid   :: B.ByteString
  , gid   :: B.ByteString
  , size  :: B.ByteString
  , hash  :: B.ByteString
  , flags :: B.ByteString
  , path  :: B.ByteString
  }

type Index = [IndexEntry]

indexPath :: FilePath
indexPath = ".git/index"

parseIndexEntry :: B.ByteString -> IndexEntry
parseIndexEntry raw = IndexEntry
  { ctime = sliced !! 0
  , mtime = sliced !! 1
  , dev   = sliced !! 2
  , ino   = sliced !! 3
  , mode  = sliced !! 4
  , uid   = sliced !! 5
  , gid   = sliced !! 6
  , size  = sliced !! 7
  , hash  = sliced !! 8
  , flags = sliced !! 9
  , path  = B.takeWhile (/= '\0') $ sliced !! 10
  }
  where slices =
          [ 8  -- ctime
          , 8  -- mtime
          , 4  -- dev
          , 4  -- ino
          , 4  -- mode
          , 4  -- uid
          , 4  -- gid
          , 4  -- size
          , 20 -- hash
          , 2  -- flags
          ]
        sliced = sliceByteString slices raw

splitRawEntries :: B.ByteString -> [B.ByteString]
splitRawEntries bs = if B.length bs ==  0
                     then []
                     else next : splitRawEntries rest
  where next  = B.pack .
                (++ "\0") .
                map snd .
                takeWhile (\(i, c) -> (i < 62)
                            || (c /= '\0')
                            || (i `mod` 8 /= 7)
                          ) .
                zip ([0..] :: [Int]) $
                B.unpack bs
        rest = B.drop (B.length next) bs

parseIndex :: B.ByteString -> Index
parseIndex index = map parseIndexEntry $
  -- drop extensions
  takeWhile (not . (`elem` [B.pack "TREE", B.pack "REUC"]) . B.take 4) .
  splitRawEntries .
  -- drop header and sha-1
  B.reverse .
  (B.drop 40) .
  B.reverse .
  (B.drop 12) $
  index

dumpIndex :: B.ByteString -> IO ()
dumpIndex index = putStrLn . intercalate "\n" . map (B.unpack . B16.encode) $
                  rawEntries
  where rawEntries = splitRawEntries .
                     B.reverse .
                     (B.drop 40) . -- drop sha-1
                     B.reverse .
                     (B.drop 12) $  -- drop header
                     index

readIndex :: IO Index
readIndex = B.readFile indexPath >>= return . parseIndex

prettyIndex :: Index -> String
prettyIndex index = intercalate "\n" $ map (B.unpack . prettyEntry) index
  where prettyEntry :: IndexEntry -> B.ByteString
        prettyEntry (IndexEntry
                     { path = path
                     , hash = hash
                     , mode = mode
                     }) = B.concat $
                          [(B16.encode mode)
                          , B.pack " "
                          , (B16.encode hash)
                          , B.pack " "
                          , path]

showIndex :: Index -> IO ()
showIndex index = putStrLn . prettyIndex $ index

unparseIndexEntry :: IndexEntry -> B.ByteString
unparseIndexEntry (IndexEntry c m d i mode u g size hash flags path) =
  B.concat [c, m, d, i, mode, u, g, size, hash, flags, path]

unparseIndex :: Index -> B.ByteString
unparseIndex index = content `B.append` hash
  where header = B.concat [dirc, version, size]
        dirc = B.pack "DIRC"
        version = B.pack . map chr $ [0, 0, 0, 2]
        size = L.toStrict . Bin.encode $ (fromIntegral . length $ index :: Int32)
        body = B.concat . map unparseIndexEntry $ index
        content = header `B.append` body
        hash = SHA1.hash $ content

storeIndex :: Index -> IO ()
storeIndex = B.writeFile indexPath . unparseIndex
