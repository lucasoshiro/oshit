module Core.Packfile where

import Core.Core

import Data.Int

import qualified Data.Binary                as Bin
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

parseIdxFile :: FilePath -> IO (Map.Map Hash Int)
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

      extractChunks :: Int -> B.ByteString -> [B.ByteString] -> [B.ByteString]
      extractChunks n b ac =
        if b == B.empty
        then ac
        else extractChunks n (B.drop n b) $ ac ++ [B.take n b]

      objs = extractChunks 20 objNames []
      offs = extractChunks 4 offsets []

  B.putStr offsets
  putStrLn ""

  return . Map.fromList . zip (map B16.encode objs)
    $ [ fromIntegral (Bin.decode . L.fromStrict $ o :: Int32)
               | o <- offs
               ]
