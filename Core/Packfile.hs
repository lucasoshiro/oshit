module Core.Packfile where

import Core.Core

import Data.Bits
import Data.Char
import Data.Int

import qualified Data.Binary                as Bin
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map
import qualified System.Directory           as Dir

type Idx = Map.Map Hash Int

data PackObjType = PackCommit | PackTree | PackBlob | PackTag | PackOfsDelta | PackRefDelta

type PackObj = (PackObjType, B.ByteString)

getPackObjType :: Int -> Maybe PackObjType
getPackObjType 1 = Just PackCommit
getPackObjType 2 = Just PackTree
getPackObjType 3 = Just PackBlob
getPackObjType 4 = Just PackTag
getPackObjType 5 = Just PackOfsDelta
getPackObjType 6 = Just PackRefDelta
getPackObjType _ = Nothing

extractChunks :: Int -> B.ByteString -> [B.ByteString] -> [B.ByteString]
extractChunks n b ac =
  if b == B.empty
  then ac
  else extractChunks n (B.drop n b) $ ac ++ [B.take n b]


parseIdxFile :: FilePath -> IO Idx
parseIdxFile path = do
  idxFile <- B.readFile path
  
  let (magic, notMagic) = B.splitAt 4 idxFile
      (version, notVersion) = B.splitAt 4 notMagic
      (fanOut, notFanOut) = B.splitAt (256 * 4) notVersion
  
      nObj :: Int32
      nObj = Bin.decode
        . L.fromStrict
        . B.drop (B.length fanOut - 4)
        $ fanOut
  
      (objNames, notObjNames) = B.splitAt (fromIntegral nObj * 20) notFanOut
      (crc32, notCrc32) = B.splitAt (fromIntegral nObj * 4) notObjNames
      (offsets, notOffsets) = B.splitAt (fromIntegral nObj * 4) notCrc32
      
      -- not implemented: packfiles >= 2 GB
      (packChecksum, idxChecksum) = B.splitAt 20 notOffsets

      objs = extractChunks 20 objNames []
      offs = extractChunks 4 offsets []

  return . Map.fromList . zip (map B16.encode objs)
    $ [ fromIntegral (Bin.decode . L.fromStrict $ o :: Int32)
      | o <- offs
      ]

parsePackFile :: FilePath -> IO B.ByteString
parsePackFile path = do
  packFile <- B.readFile path

  let (magic, notMagic) = B.splitAt 4 packFile
      (version, notVersion) = B.splitAt 4 notMagic
      (size, notSize) = B.splitAt 4 notVersion
      (content, checksum) = B.splitAt (B.length notVersion - 20) notSize
  
  return content

extractNonDeltified :: B.ByteString -> (Maybe PackObjType, B.ByteString)
extractNonDeltified dataStart = (objType, B.drop headerSize dataStart)
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
    relevantData = (0xf .&. (ord . head $ header)) : [shift (ord d) 1 | d <- tail header]
    enumerated = zip [0..] relevantData
    shifted = [(shift d (i - 8), shift d i) | (i, d) <- enumerated]
    joined = zipWith (\ a b -> (snd a) .|. (fst b)) shifted (tail shifted)
    size = sum . map (\(a, b) -> a * 256 ^ b) $ zip joined [0..] 

getObjectData :: Idx -> B.ByteString -> Hash -> Maybe PackObj
getObjectData idx packfile hash = do
  offset <- Map.lookup hash idx
  let dataStart = B.drop (offset - 0xc) packfile

  let (objType', extracted) = extractNonDeltified $ dataStart
  objType <- objType'

  -- non-deltified only
  return (objType, extracted)

searchInPackFile :: Hash -> Hash -> IO (Maybe PackObj)
searchInPackFile packHash objHash = do
  let packHashStr = B.unpack packHash
      idxName = concat [".git/objects/pack/pack-", packHashStr, ".idx"]
      packName = concat [".git/objects/pack/pack-", packHashStr, ".pack"]

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
