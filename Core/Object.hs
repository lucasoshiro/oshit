module Core.Object where

import qualified Codec.Compression.Zlib     as Zlib
import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified System.Directory           as Dir

data Object = Blob B.ByteString

objectContent :: Object -> B.ByteString
objectContent (Blob c) = c

objectType :: Object -> B.ByteString
objectType (Blob _) = B.pack "blob"

objectParse :: B.ByteString -> Object
objectParse bs = constructor objType content
  where objType = B.unpack $ (B.split ' ' bs) !! 0
        content = (B.split '\0' bs) !! 1
        constructor "blob" = Blob
        constructor _ = Blob

objectUnparse :: Object -> B.ByteString
objectUnparse obj = B.concat $
  [ objType, B.pack " ", size, B.pack "\0"
  , content
  ]
  where objType = objectType obj
        content = objectContent obj
        size = B.pack $ show $ B.length content

hashObject :: Object -> B.ByteString
hashObject = B16.encode . SHA1.hash . objectUnparse

hashString :: String -> B.ByteString
hashString = hashObject . Blob . B.pack

compressObject :: Object -> B.ByteString
compressObject = L.toStrict . Zlib.compress . L.fromStrict . objectUnparse

decompressObject :: B.ByteString -> Object
decompressObject = objectParse . L.toStrict . Zlib.decompress . L.fromStrict

storeObject :: Object -> IO ()
storeObject obj = do
  Dir.createDirectoryIfMissing True completeDir
  B.writeFile path compressed
  where hashStr = B.unpack $ hashObject obj
        dir = take 2 $ hashStr
        filename = drop 2 $ hashStr
        completeDir = concat [".git/objects/", dir, "/"]
        path = completeDir ++ filename
        compressed = compressObject obj

loadObject :: B.ByteString -> IO Object
loadObject hash = B.readFile path >>= return . decompressObject
  where hashStr = B.unpack hash
        dir = take 2 $ hashStr
        filename = drop 2 $ hashStr
        path = concat [".git/objects/", dir, "/", filename]
