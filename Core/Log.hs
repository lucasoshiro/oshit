module Core.Log where

import Core.Core
import Core.Object.Object
import Core.Object.Commit ()

import Data.Set as Set

commitBFS :: Hash -> IO [(Hash, Commit)]
commitBFS hash = commitBFS' [hash] Set.empty

commitBFS' :: [Hash] -> Set Hash -> IO [(Hash, Commit)]
commitBFS' (hash:queue) visited = do
  if hash `Set.member` visited
    then return []
    else do
    rawObj <- loadRawObject hash
    obj <- objectParse rawObj :: IO Commit
    let (Commit _ commitParents _ _ _ _) = obj
    let descendents = commitBFS' (queue ++ commitParents) (Set.insert hash visited)
    let current = return [(hash, obj)] :: IO [(Hash, Commit)]
  
    (++) <$> current <*> descendents
commitBFS' [] _ = return []
