module Core.Merge where

import Core.Core
import Core.Object

import Data.List

import qualified Data.Set as Set
import qualified Data.Map as Map

-- Merge result: if it **is** Nothing, there's a conflict. If it **contains**
-- Nothing, the file was deleted.
type MergeResult = Maybe (Maybe Hash)

threeWayMerge :: Maybe Hash -> Maybe Hash -> Maybe Hash -> MergeResult
threeWayMerge o a b
  | a == b = Just a
  | a == o = Just b
  | b == o = Just a
  | otherwise = Nothing

mergeTree :: TreeIO -> TreeIO -> TreeIO -> TreeIO
mergeTree o a b = do
  -- Just handling content merge. File permission and submodule merge not
  -- implemented yet. We're not detecting renames. By now we're just giving
  -- up if there are conflicts.

  contentsO <- treeContentsRecursive o
  contentsA <- treeContentsRecursive a
  contentsB <- treeContentsRecursive b

  let fileMapO = Map.fromList [(path, hash) | (_, path, hash) <- contentsO]
  let fileMapA = Map.fromList [(path, hash) | (_, path, hash) <- contentsA]
  let fileMapB = Map.fromList [(path, hash) | (_, path, hash) <- contentsB]

  let filesO = Set.fromList . Map.keys $ fileMapO
  let filesA = Set.fromList . Map.keys $ fileMapA
  let filesB = Set.fromList . Map.keys $ fileMapB

  let allFilesNames = filesO `Set.union` filesA `Set.union` filesB
  let allFiles = [ ( path
                   , ( Map.lookup path fileMapO
                     , Map.lookup path fileMapA
                     , Map.lookup path fileMapB
                     )
                   )
                 | path <- Set.toList allFilesNames
                 ]

  let merges = [ (path, threeWayMerge o' a' b')
               | (path, (o', a', b')) <- allFiles
               ]

  let conflicts = [path | (path, merge) <- merges, merge == Nothing]

  if conflicts /= []
    then fail $ intercalate "\n" ("conflicts: ":conflicts)
    else return ()

  let merged = merges >>= \(path, merge) -> case merge of
        Just (Just hash) -> [(StdMode, path, hash)] -- merged ok
        Just Nothing     -> []                      -- deleted file
        Nothing          -> []                      -- conflict

  let root:descendents = treesFromContents merged

  _ <- sequence . reverse $ descendents
  root
