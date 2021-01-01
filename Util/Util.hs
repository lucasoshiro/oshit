module Util.Util where

import qualified Data.Binary                as Bin
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

import Data.Char (isSpace)
import Data.Int

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
