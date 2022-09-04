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

-- | Inverts a 'Map.Map' by mapping its values to their /preimage/ in the
-- original 'Map.Map'. The order of the original keys in the preimage is not
-- guaranteed, unless by 'foldl'.
--
-- >>> invertMap $ fromList [(1, 3), (2, 3), (10, 1)]
-- fromList [(1,[10]),(3,[2,1])]
invertMap :: Ord b => Map.Map a b -> Map.Map b [a]
invertMap m = foldl addInverted initial (Map.toList m)
  where initial = Map.fromList [ (v, []) | (_, v) <- Map.toList m]
        addInverted inverted (k, v) = Map.insertWith (++) v [k] inverted

-- | Trim whitespace from the ends of a 'String'.
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

-- | Serialize 'Bin.Binary' types into a strict 'B.ByteString'.
toByteString :: Bin.Binary b => b -> B.ByteString
toByteString = L.toStrict . Bin.encode

-- | Convert an 'Integral' type into a 32-bit integer. Beware of overflows.
to32 :: Integral a => a -> Int32
to32 = fromIntegral

-- | Convert an 'Integral' type into a 64-bit integer. Beware of overflows.
to64 :: Integral a => a -> Int64
to64 = fromIntegral

-- | Serialize an 'Integral' type into a 'B.ByteString' by first converting it
-- into a 32-bit integer. Beware of overflows.
toBS32 :: Integral a => a -> B.ByteString
toBS32 = toByteString . to32

-- | Serialize an 'Integral' type into a 'B.ByteString' by first converting it
-- into a 64-bit integer. Beware of overflows.
toBS64 :: Integral a => a -> B.ByteString
toBS64 = toByteString . to64

-- | Produce a list with the paths to all directories and files under a given
-- 'FilePath', recursively.
listDirectoryRecursive :: FilePath -> IO [FilePath]
listDirectoryRecursive path = do
  contents <- map ((path ++ "/") ++) . delete ".git" <$> listDirectory path
  directories <- filterM doesDirectoryExist contents
  recursiveList <- join <$> mapM listDirectoryRecursive directories
  return $ contents ++ recursiveList

-- | Produce a 'Map.Map' out of the common keys of two given maps, where the
-- value at key @k@ is @(a,b)@, where @a@ is the value at @k@ in the first map,
-- and @b@ is the value at @k@ in the second map.
--
-- >>> zipMap (fromList [(1, "a"), (2, "b")]) (fromList [(2, "x"), (3, "y")])
-- fromList [(2,("b","x"))]
zipMap :: Ord k => Map.Map k a -> Map.Map k b -> Map.Map k (a, b)
zipMap = Map.intersectionWith (,)

-- | Read a octal number written as a 'String' into an 'Int'.
parseOctal :: String -> Int
parseOctal = read . ("0o" ++)
