module Core.Object.Object where

import Data.Time
import qualified Codec.Compression.Zlib     as Zlib
import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified System.Directory           as Dir

import Core.Core

type ObjectType = B.ByteString
type FileMode = String

data Tree = Tree [(FileMode, FilePath, Hash)]
data Blob = Blob B.ByteString
data Commit = Commit
  { treeHash  :: Hash
  , parents   :: [Hash]
  , author    :: String
  , email     :: String
  , timestamp :: ZonedTime
  , message   :: String
  }

class Object obj where
  objectType       :: obj -> ObjectType
  objectParse      :: B.ByteString -> IO obj
  objectRawContent :: obj -> B.ByteString
  objectPretty     :: obj -> String

hashObject :: Object obj => obj -> Hash
hashObject = B16.encode . SHA1.hash . objectFileContent

compress :: B.ByteString -> B.ByteString
compress = L.toStrict . Zlib.compress . L.fromStrict

decompress :: B.ByteString -> B.ByteString
decompress = L.toStrict . Zlib.decompress . L.fromStrict

storeObject :: Object obj => obj -> IO ()
storeObject obj = do
  Dir.createDirectoryIfMissing True completeDir
  B.writeFile path compressed
  where hashStr      = B.unpack $ hashObject obj
        dir          = take 2 $ hashStr
        filename     = drop 2 $ hashStr
        completeDir  = concat [".git/objects/", dir, "/"]
        path         = completeDir ++ filename
        uncompressed = objectFileContent obj
        compressed   = compress uncompressed

loadObject :: Object obj => Hash -> IO obj
loadObject hash = loadRawObject hash >>= objectParse

loadRawObject :: Hash -> IO B.ByteString
loadRawObject hash = B.readFile (hashPath hash) >>= return . decompress

objectFileContent :: Object obj => obj -> B.ByteString
objectFileContent obj = uncompressed
  where content      = objectRawContent obj
        size         = B.pack $ show $ B.length content
        objType      = objectType obj
        uncompressed = B.concat [objType, B.pack " ", size, B.pack "\0", content]

rawObjectType :: B.ByteString -> ObjectType
rawObjectType = B.takeWhile (/= ' ')

hashPath :: Hash -> FilePath
hashPath hash = path
  where hashStr  = B.unpack hash
        dir      = take 2 $ hashStr
        filename = drop 2 $ hashStr
        path     = concat [".git/objects/", dir, "/", filename]

objectExists :: String -> IO Bool
objectExists hashStr = do
  let path = ".git/objects/" ++ take 2 hashStr ++ "/" ++ drop 2 hashStr
  Dir.doesFileExist path
