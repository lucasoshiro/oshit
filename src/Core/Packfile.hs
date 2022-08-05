module Core.Packfile where

import Core.Core
import Util.Util

import Data.Bits
import Data.Char
import Data.Int
import Data.Maybe

import qualified Data.Binary                as Bin
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map
import qualified System.Directory           as Dir

type Idx = Map.Map Hash Int

data PackObjType = PackCommit | PackTree | PackBlob | PackTag | PackOfsDelta | PackRefDelta

type PackObj = (PackObjType, Int, B.ByteString)

getPackObjType :: Int -> Maybe PackObjType
getPackObjType 1 = Just PackCommit
getPackObjType 2 = Just PackTree
getPackObjType 3 = Just PackBlob
getPackObjType 4 = Just PackTag
getPackObjType 5 = Just PackOfsDelta
getPackObjType 6 = Just PackRefDelta
getPackObjType _ = Nothing

instance Show PackObjType where
  show PackCommit   = "commit"
  show PackTree     = "tree"
  show PackBlob     = "blob"
  show PackTag      = "tag"
  show PackOfsDelta = "ofs"
  show PackRefDelta = "ref"

extractChunks :: Int -> B.ByteString -> [B.ByteString] -> [B.ByteString]
extractChunks n b ac =
  if b == B.empty
  then ac
  else extractChunks n (B.drop n b) $ ac ++ [B.take n b]

-- Not the best solution, as an elegant one is hard as hell to implement
parseSize :: B.ByteString -> Int
parseSize bs = foldl (\ac b -> ac * (2 ^ length b) + (ord . boolListToByte $ b)) 0 groups
  where bools = map byteToBoolList $ B.unpack bs
        ones = takeWhile last bools
        useful = ones ++ [
          bools !! (length ones)
          ++ (take (length ones) . repeat $ False)
          ]
        inits = map init useful
        flat = drop 3 $ inits >>= reverse -- 3 bits for type
        groups = (reverse flat) `groupsOf` 8

parseIdxFile :: FilePath -> IO Idx
parseIdxFile path = do
  idxFile <- B.readFile path
  
  let (_, notMagic) = B.splitAt 4 idxFile
      (_, notVersion) = B.splitAt 4 notMagic
      (fanOut, notFanOut) = B.splitAt (256 * 4) notVersion
  
      nObj :: Int32
      nObj = Bin.decode
        . L.fromStrict
        . B.drop (B.length fanOut - 4)
        $ fanOut
  
      (objNames, notObjNames) = B.splitAt (fromIntegral nObj * 20) notFanOut
      (_, notCrc32) = B.splitAt (fromIntegral nObj * 4) notObjNames
      (offsets, _) = B.splitAt (fromIntegral nObj * 4) notCrc32
      
      -- not implemented: packfiles >= 2 GB
      -- (packChecksum, idxChecksum) = B.splitAt 20 notOffsets

      objs = extractChunks 20 objNames []
      offs = extractChunks 4 offsets []

  return . Map.fromList . zip (map B16.encode objs)
    $ [ fromIntegral (Bin.decode . L.fromStrict $ o :: Int32)
      | o <- offs
      ]

parsePackFile :: FilePath -> IO B.ByteString
parsePackFile path = do
  packFile <- B.readFile path

  let (_, notMagic) = B.splitAt 4 packFile
      (_, notVersion) = B.splitAt 4 notMagic
      (_, notSize) = B.splitAt 4 notVersion
      (content, _) = B.splitAt (B.length notVersion - 20) notSize
  
  return content

extractNonDeltified :: B.ByteString -> (Maybe PackObjType, Int, B.ByteString)
extractNonDeltified dataStart = (objType, size, B.drop headerSize dataStart)
  where 
    firstBit :: Char -> Bool
    firstBit = (== 0x80) . (.&. 0x80) . ord
    getHeader :: B.ByteString -> String
    getHeader b = if firstBit . B.head $ b
                  then (B.head b) : (getHeader . B.tail $ b)
                  else [B.head b]
    header = getHeader dataStart
    objType = getPackObjType . (flip shift $ (-4)) . (0x70 .&. ) . ord . head $ header
    headerSize = length header
    size = parseSize . B.pack $ header
    

getObjectData :: Idx -> B.ByteString -> Hash -> Maybe PackObj
getObjectData idx packfile hash = Map.lookup hash idx >>= getObjectInOffset packfile

getObjectInOffset :: B.ByteString -> Int -> Maybe PackObj
getObjectInOffset packfile offset = do
  let dataStart = B.drop (offset - 0xc) packfile

  let (objType', size, extracted) = extractNonDeltified $ dataStart
  objType <- objType'

  -- non-deltified only
  return (objType, size, extracted)
  

idxPath :: Hash -> String
idxPath hash = concat [".git/objects/pack/pack-", B.unpack hash, ".idx"]
  
packPath :: Hash -> String
packPath hash = concat [".git/objects/pack/pack-", B.unpack hash, ".pack"]

searchInPackFile :: Hash -> Hash -> IO (Maybe PackObj)
searchInPackFile packHash objHash = do
  let idxName = idxPath packHash
      packName = packPath packHash

  idx <- parseIdxFile idxName
  pack <- parsePackFile packName

  return $ getObjectData idx pack objHash

listPackFiles :: IO [Hash]
listPackFiles = do
  files <- Dir.listDirectory ".git/objects/pack"

  let hashes = [ B.pack . take 40 . drop (length f - 45) $ f
               | f <- files, drop (length f - 5) f == ".pack"
               ]
  return hashes

searchInPackFiles :: Hash -> IO (Maybe PackObj)
searchInPackFiles hash = do
  packs <- listPackFiles
  search <- sequence [searchInPackFile pack hash | pack <- packs]
  let found = do
        s <- search
        case s of
          (Just p) -> [p]
          Nothing  -> []

  return $ case found of
    [] -> Nothing
    p  -> Just . head $ p

verifyPack :: Hash -> IO [(Hash, PackObjType, Int)]
verifyPack hash = do
  let idxName = idxPath hash
  idx <- parseIdxFile idxName

  let objInfo h = d >>= \(t, s, _) -> return (h, t, s)
        where d = searchInPackFiles h >>= return . fromJust
      objHashes = Map.keys idx

  sequence $ map objInfo objHashes
