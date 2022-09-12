--------------------------------------------------------------------------------
-- |
-- Module      :  Core.Object.Core
-- Copyright   :  (c) Lucas Oshiro 2022
--
-- Maintainer  : lucasseikioshiro@gmail.com
--
-- This module defines the Object typeclass as well as some functions that load
-- objects off and store objects to the disk.
--------------------------------------------------------------------------------

module Core.Object.Core where

import Control.Monad.Extra (unlessM)
import Data.ByteString.Char8 (ByteString, unpack, pack)
import System.Directory (createDirectoryIfMissing, doesDirectoryExist, doesFileExist)
import System.FilePath (takeDirectory)
import Text.Read (readMaybe)

import qualified Crypto.Hash.SHA1       as SHA1
import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as B

import Core.Core (Hash)
import Core.Packfile (PackObjType(..), searchInPackFiles)
import Util.Util (compress, decompress)

-- | Typeclass for Git objects.
class Object o where
  -- | Returns the object type.
  objectType :: o -> ObjectType

  -- | Parse a 'ByteString' into an object of the given type.
  objectParse :: ByteString -> IO o

  -- | Serialize the object into a 'ByteString'.
  objectRawContent :: o -> ByteString

  -- | Pretty print the object.
  objectPretty :: o -> String

-- | Helper type to enumerate valid Git object types.
data ObjectType = BlobType | TreeType | CommitType

instance Read ObjectType where
  readsPrec _ "blob"   = [(BlobType,   "")]
  readsPrec _ "tree"   = [(TreeType,   "")]
  readsPrec _ "commit" = [(CommitType, "")]
  readsPrec _ _        = []

instance Show ObjectType where
  show BlobType   = "blob"
  show TreeType   = "tree"
  show CommitType = "commit"

gitTimeFormat :: String
gitTimeFormat = "%s %z"

-- | Tries to parse the type of a Git object from its uncompressed contents.
rawObjectType :: B.ByteString -> Maybe ObjectType
rawObjectType = readMaybe . B.unpack . B.takeWhile (/= ' ')

-- | Compute the SHA1 hash of an object.
hashObject :: Object o => o -> Hash
hashObject = B16.encode . SHA1.hash . objectFileContent

-- | Returns the path to the object file given its hash.
--
-- >>> hashPath "aabbccddeeff"
-- ".git/objects/aa/bbccddeeff"
hashPath :: Hash -> FilePath
hashPath hash = concat [".git/objects/", dir, "/", filename]
  where (dir, filename) = splitAt 2 $ unpack hash

-- | Stores a compressed Git object on disk.
--
-- TODO: Support packfiles.
storeObject :: Object o => o -> IO ()
storeObject obj = do
  let objectPath = hashPath $ hashObject obj
  createDirectoryIfMissing True $ takeDirectory objectPath
  unlessM (doesDirectoryExist objectPath) $ do
    B.writeFile objectPath $ compress (objectFileContent obj)

-- | Loads a Git object from disk (either loose or packed) as a 'B.ByteString'.
loadRawObject :: Hash -> IO B.ByteString
loadRawObject hash = do
  exists <- looseObjectExists . B.unpack $ hash
  if exists
    then loadLooseRawObject $ hash
    else do
      packed <- loadPackedRawObject hash

      case packed of
        Just b -> return b
        Nothing -> fail "object not found"

-- | Loads a Git object from disk and attempts to parse it.
loadObjectLegacy :: Object o => Hash -> IO o
loadObjectLegacy hash = loadRawObject hash >>= objectParse

-- | Loads a loose Git object from disk as a 'B.ByteString'.
loadLooseRawObject :: Hash -> IO B.ByteString
loadLooseRawObject hash = B.readFile (hashPath hash) >>= return . decompress

-- | Loads a packed Git object from disk as a 'Maybe'@ @'B.ByteString'.
loadPackedRawObject :: Hash -> IO (Maybe B.ByteString)
loadPackedRawObject hash = do
  obj <- searchInPackFiles hash
  return $ do
    (objType, content) <- obj

    typeStr <- case objType of
                    PackBlob   -> Just . B.pack $ "blob"
                    PackTree   -> Just . B.pack $ "tree"
                    PackCommit -> Just . B.pack $ "commit"
                    _          -> Nothing

    let decompressed = decompress content
        size = B.length decompressed

    return . B.concat $ [ typeStr
                        , B.pack " "
                        , B.pack . show $ size
                        , B.pack "\0"
                        , decompressed
                        ]

-- | Determines if a loose object file exists given its hash as a string.
looseObjectExists :: String -> IO Bool
looseObjectExists = doesFileExist . hashPath . pack

-- | Get the uncompressed contents of an object as a 'ByteString'.
objectFileContent :: Object o => o -> ByteString
objectFileContent obj = uncompressed
  where content      = objectRawContent obj
        size         = pack $ show $ B.length content
        objType      = objectType obj
        uncompressed = B.concat [ pack $ show objType
                                , pack " "
                                , size
                                , pack "\0", content]
