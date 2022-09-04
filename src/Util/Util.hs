module Util.Util where

import qualified Data.Binary                as Bin
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map

import Control.Monad (filterM, join)
import Data.Char (isSpace)
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

-- | Divide a 'B.ByteString' into a list of 'B.ByteString' slices according to
-- a list of spans / lengths. Each 'B.ByteString' in the resulting list will
-- have length equal to the span that generated it. If the original string is
-- longer than the sum of all spans, the remaining slice will be added to the
-- end of the resulting slice list.
--
-- >>> sliceByteString [1, 2, 3] "abcdef"
-- ["a","bc","def"]
--
-- >>> sliceByteString [1, 2] "abcdef"
-- ["a","bc","def"]
--
-- >>> sliceByteString [1, 2, 3, 4] "abc"
-- ["a","bc","",""]
sliceByteString :: [Int] -> B.ByteString -> [B.ByteString]
sliceByteString [] bs = [ bs | bs /= B.empty ]
sliceByteString (s:ss) bs = B.take s bs : sliceByteString ss (B.drop s bs)

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
  contents <- map ((path ++ "/") ++) . delete ".git" <$> listDirectory path
  directories <- filterM doesDirectoryExist contents
  recursiveList <- join <$> mapM listDirectoryRecursive directories
  return $ contents ++ recursiveList

zipMap :: Ord k => Map.Map k a -> Map.Map k b -> Map.Map k (a, b)
zipMap = Map.intersectionWith (,)

parseOctal :: String -> Int
parseOctal = read . ("0o" ++)
