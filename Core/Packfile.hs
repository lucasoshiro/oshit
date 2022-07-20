module Core.Packfile where

import Core.Core

import Data.Bits
import Data.Char
import Data.Int
import Data.List
import Numeric

import qualified Data.Binary                as Bin
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

type Idx = Map.Map Hash Int

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

extractNonDeltified :: B.ByteString -> B.ByteString
extractNonDeltified dataStart = B.drop headerSize dataStart
  where 
    firstBit :: Char -> Bool
    firstBit = (== 0x80) . (.&. 0x80) . ord
    getHeader :: B.ByteString -> String
    getHeader b = if firstBit . B.head $ b
                  then (B.head b) : (getHeader . B.tail $ b)
                  else [B.head b]

    headerSize = length . getHeader $ dataStart

getObjectData :: Idx -> B.ByteString -> Hash -> Maybe B.ByteString
getObjectData idx packfile hash = do
  offset <- Map.lookup hash idx
  let dataStart = B.drop (offset - 0xc) packfile

  -- non-deltified only
  return . extractNonDeltified $ dataStart
