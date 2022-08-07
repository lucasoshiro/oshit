module Core.Packfile where

import Core.Core
import Util.Util
import Util.Compression

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

firstBit :: Char -> Bool
firstBit = (== 0x80) . (.&. 0x80) . ord

parsePackFile :: FilePath -> IO B.ByteString
parsePackFile path = do
  packFile <- B.readFile path

  let (_, notMagic) = B.splitAt 4 packFile
      (_, notVersion) = B.splitAt 4 notMagic
      (_, notSize) = B.splitAt 4 notVersion
      (content, _) = B.splitAt (B.length notVersion - 20) notSize
  
  return content

getObjectData :: Idx -> B.ByteString -> Hash -> Maybe PackObj
getObjectData idx packfile hash = Map.lookup hash idx >>= getObjectInOffset packfile

getSizeHeader :: B.ByteString -> String
getSizeHeader b = if firstBit . B.head $ b
                  then (B.head b) : (getSizeHeader . B.tail $ b)
                  else [B.head b]

getObjectInOffset :: B.ByteString -> Int -> Maybe PackObj
getObjectInOffset packfile offset = do
  let dataStart = B.drop (offset - 0xc) packfile
      header = getSizeHeader dataStart
      objType' = getPackObjType . (flip shift $ (-4)) . (0x70 .&. ) . ord . head $ header
      headerSize = length header
      size = parseSize . B.pack $ header

  objType <- objType'

  content <- case objType of
    PackOfsDelta -> fail "ofs not implemented yet"
    PackRefDelta -> fail "ref not implemented yet"
    _ -> return . decompress $ B.drop headerSize dataStart

  -- non-deltified only
  return (objType, size, content)
  
readRefDelta :: B.ByteString -> Int -> IO (B.ByteString, PackObjType)
readRefDelta bs size = do
  let hash' = B.take 20 bs
      hash = B16.encode hash'
      remaining = B.drop 20 bs

  baseObj <- searchInPackFiles $ hash

  readDelta remaining baseObj (size - 20)
  
readDelta :: B.ByteString -> Maybe PackObj -> Int -> IO (B.ByteString, PackObjType)
readDelta bs baseObj size' = do
  (objType, _, baseData) <- case baseObj of
    (Just o) -> return o
    Nothing  -> fail "object not found in packfiles"

  let stream = decompress bs

      baseSize' = getSizeHeader stream
      objSize' = getSizeHeader
        . B.drop (length baseSize')
        . B.pack
        $ baseSize'

      baseSize = parseSize . B.pack $ baseSize'
      objSize = parseSize . B.pack $ objSize'

      size = size' - (length baseSize' + length objSize')
      remaining = B.take size
        . B.drop (length baseSize' + length objSize')
        $ bs

  return (applyDeltaInstructions baseData remaining, objType)

applyDeltaInstructions :: B.ByteString -> B.ByteString -> B.ByteString
applyDeltaInstructions base bs = B.empty
  where fstBit = firstBit . B.head $ bs
        

applyData :: B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
applyData input bs = (output, remaining)
  where size = (.&. 0x7f) . ord . B.head $ bs
        data' = B.take size . B.drop 1 $ bs
        output = B.pack $ B.unpack input ++ B.unpack data'
        remaining = B.drop (size + 1) bs

applyCopy :: B.ByteString -> B.ByteString -> B.ByteString -> (B.ByteString, B.ByteString)
applyCopy input bs base = (output, remaining)
  where (off, dataSize, size) = readOffsetAndSize bs
        copied = B.take dataSize . B.drop off $ base
        output = B.pack $ B.unpack input ++ B.unpack copied
        remaining = B.drop size bs

readOffsetAndSize :: B.ByteString -> (Int, Int, Int)
readOffsetAndSize bs = (off, dataSize, size)
  where fstBits = byteToBoolList . B.head $ bs
        offBits = reverse . take 3 . drop 4 $ fstBits
        dataSizeBits = reverse . take 4 $ fstBits

        nOff = sum [if b then 1 else 0 | b <- offBits]
        nObjSize = sum [if b then 1 else 0 | b <- dataSizeBits]

        offBytes = take nOff . drop 1 . B.unpack $ bs
        dataSizeBytes = take nObjSize . drop (nOff + 1) . B.unpack $ bs

        readWeirdFormat :: [Bool] -> [Char] -> [Char]
        readWeirdFormat [] _ = []
        readWeirdFormat (True:bits) (c:cs) = c : (readWeirdFormat bits cs)
        readWeirdFormat (False:bits) cs = '\0' : (readWeirdFormat bits cs)

        offExpanded = readWeirdFormat offBits offBytes
        dataSizeExpanded = readWeirdFormat dataSizeBits dataSizeBytes

        off = foldl1 (\ac b -> ac * 256 + b) $ map ord offExpanded
        dataSize = foldl1 (\ac b -> ac * 256 + b) $ map ord dataSizeBytes

        size = 1 + nOff + nObjSize

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
