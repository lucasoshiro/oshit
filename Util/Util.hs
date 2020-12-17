module Util.Util where

import qualified Data.Map as Map
import Data.Char (isSpace)
  
invertMap :: Ord b => Map.Map a b -> Map.Map b [a]
invertMap m = foldl addInverted initial (Map.toList m)
  where initial = Map.fromList [ (v, []) | (_, v) <- Map.toList m]
        addInverted inverted (k, v) = Map.insertWith (++) v [k] inverted

trim :: String -> String
trim = f . f
   where f = reverse . dropWhile isSpace
