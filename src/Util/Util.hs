module Util.Util where

import qualified Data.Binary                as Bin
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

import Data.Bits
import Data.Char (chr, isSpace, ord)
import Data.Int
import Data.List

import System.Directory

invertMap :: Ord b => Map.Map a b -> Map.Map b [a]
invertMap m = foldl addInverted initial (Map.toList m)
  where initial = Map.fromList [ (v, []) | (_, v) <- Map.toList m]
        addInverted inverted (k, v) = Map.insertWith (++) v [k] inverted

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace

sliceByteString :: [Int] -> B.ByteString -> [B.ByteString]
sliceByteString [] bs = if bs /= B.empty then [bs] else []
sliceByteString (size:rest) bs = (B.take size bs) :
                                 (sliceByteString rest $ B.drop size bs)

toByteString :: Bin.Binary b => b -> B.ByteString
toByteString = L.toStrict . Bin.encode

to32 :: Integral a => a -> Int32
to32 = fromIntegral

to64 :: Integral a => a -> Int64
to64 = fromIntegral

toBS32 :: Integral a => a -> B.ByteString
toBS32 = toByteString . to32

toBS64 :: Integral a => a -> B.ByteString
toBS64 = toByteString . to64

listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
  contents <- getDirectoryContents path >>= return . (\\ [".", "..", ".git"])
  let x :: IO [[FilePath]]
      x = sequence $ do
        content <- contents
        let childPath = path ++ "/" ++ content :: FilePath
        return $ do
          isDir <- doesDirectoryExist childPath
          if isDir
            then listDirectoryRecursive childPath >>= return . (childPath :)
            else return [childPath] -- IO [FilePath]
            
  x >>= return . (>>= id)

zipMap :: Ord k => Map.Map k a -> Map.Map k b -> Map.Map k (a, b)
zipMap m1 m2 = Map.mapWithKey (\k v1 -> (v1, m2 Map.! k)) m1

parseOctal :: String -> Int
parseOctal = read . ("0o" ++)

byteToBoolList :: Char -> [Bool]
byteToBoolList b = map (b' `testBit`) [0..7]
  where b' = ord b

boolListToByte :: [Bool] -> Char
boolListToByte bs = chr $ foldl f 0 bs'
  where f a b = (if b then 0x80 else 0) + (a `shiftR` 1)
        bs'   = bs ++ (take (8 - length bs) . repeat $ False)

groupsOf :: [a] -> Int -> [[a]]
groupsOf [] _ = []
groupsOf l n = (take n l) : groupsOf (drop n l) n
