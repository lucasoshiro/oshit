--------------------------------------------------------------------------------
-- |
-- Module      :  Core.Object
-- Copyright   :  (c) Lucas Oshiro 2022
--
-- Maintainer  : lucasseikioshiro@gmail.com
--
-- This module exports the definitions of Git objects.
--------------------------------------------------------------------------------

module Core.Object (
    module Core.Object.Core,
    module Core.Object.Tree,
    module Core.Object.Commit,
    module Core.Object.Blob,

    -- temporary
    Tree(..),
    TreeIO,
    CommitIO,
    listTreeRecursive,
    loadTree,
    treeContentsRecursive,
    treesFromContents,
    insertToInnerTree,
    InnerTreeNode(..),
    loadBlob,
    loadCommit,
    treesFromInnerTrees
  ) where

import Core.Object.Core
import Core.Object.Blob
import Core.Object.Commit
import Core.Object.Tree

import Data.Either
import Data.List
import Data.List.Split

import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.Map                   as Map

import Core.Core

type BlobIO   = IO Blob
type TreeIO   = IO Tree
type CommitIO = IO Commit

data ObjectIO = ObjectIO BlobIO TreeIO CommitIO

data InnerTreeNode = InnerLeaf Hash
                   | InnerTree FilePath (Map.Map FilePath InnerTreeNode)

data HashedInnerTree = HashedInnerTree (Hash, Tree, [HashedInnerTree])

-- | Loads a Git object from disk as an 'ObjectIO'.
loadObject :: Hash -> ObjectIO
loadObject hash = ObjectIO blobIO treeIO commitIO
  where raw      = loadRawObject hash
        blobIO   = raw >>= objectParse
        treeIO   = raw >>= objectParse
        commitIO = raw >>= objectParse

-- | Loads a Git blob from disk.
loadBlob :: Hash -> BlobIO
loadBlob hash = blobIO
  where (ObjectIO blobIO _ _) = loadObject hash

-- | Loads a Git tree from disk.
loadTree :: Hash -> TreeIO
loadTree hash = treeIO
  where (ObjectIO _ treeIO _) = loadObject hash

-- | Loads a Git commit from disk.
loadCommit :: Hash -> CommitIO
loadCommit hash = commitIO
  where (ObjectIO _ _ commitIO) = loadObject hash

-- Tree

insertToInnerTree :: InnerTreeNode
                  -> (Hash, [FilePath], FilePath)
                  -> InnerTreeNode
insertToInnerTree (InnerTree treeName nodes) (hash, [], childName) =
  InnerTree treeName $
  Map.insert childName (InnerLeaf hash) nodes
insertToInnerTree (InnerTree treeName nodes) (hash, root:innerPath, childName) =
  InnerTree treeName $
  Map.insertWith joinTree root newChild nodes
  where newChild = insertToInnerTree emptyInnerTree (hash, innerPath, childName)
        joinTree (InnerTree n a) (InnerTree _ b) = InnerTree n $ a `Map.union` b
        joinTree _ x = x
        emptyInnerTree = InnerTree root Map.empty
insertToInnerTree _ _ = InnerTree "" Map.empty

treesFromInnerTrees :: InnerTreeNode -> [Tree]
treesFromInnerTrees (InnerTree _ nodes) = (Tree content) : descendentTrees
  where nodeList = Map.toList nodes

        blobEntries :: [(FileMode, FilePath, Hash)]
        blobEntries = do
          (name, child) <- nodeList
          case child of (InnerTree _ _) -> []
                        (InnerLeaf hash) -> [
                          (StdMode, name, (fromRight B.empty . B16.decode $ hash))
                          ]

        descendentTrees' :: [(FilePath, [Tree])]
        descendentTrees' = [ (name, [c | c <- treesFromInnerTrees child])
                           | (name, child) <- nodeList]

        childTrees :: [(FilePath, Tree)]
        childTrees = [ (name, head trees)
                     | (name, trees) <- descendentTrees'
                     , length trees > 0
                     ]

        descendentTrees :: [Tree]
        descendentTrees = descendentTrees' >>= snd

        treeEntries :: [(FileMode, FilePath, Hash)]
        treeEntries = do
          (name, tree) <- childTrees
          [ (DirMode, name, (fromRight B.empty . B16.decode . hashObject $ tree)) ]

        content = blobEntries ++ treeEntries
treesFromInnerTrees _ = []

treeContentsRecursive :: TreeIO -> IO [(FileMode, FilePath, Hash)]
treeContentsRecursive = (>>= \(Tree entries) -> treeContentsRecursive' [] entries)

treeContentsRecursive' :: [String] -> [(FileMode, FilePath, Hash)] -> IO [(FileMode, FilePath, Hash)]
treeContentsRecursive' _ [] = return []
treeContentsRecursive' path ((entryMode, name, entryHash):rest) =
  do
    let fullPath = path ++ [name]
    let rawFullPath = intercalate "/" fullPath

    let subList = case entryMode of
          DirMode -> loadTree entryHash >>= \(Tree entries) -> treeContentsRecursive' fullPath entries
          StdMode -> return $ [(StdMode, rawFullPath, entryHash)]

    it <- subList
    r <- treeContentsRecursive' path rest
    return $ it ++ r

listTreeRecursive :: TreeIO -> IO [FilePath]
listTreeRecursive treeIO = do
  contents <- treeContentsRecursive treeIO
  return [path | (_, path, _) <- contents]

treesFromContents :: [(FileMode, FilePath, Hash)] -> [TreeIO]
treesFromContents contents = [storeObject tree >> return tree | tree <- trees]
  where splittedIndex = [ (hash, splitOn "/" path)
                        | (_, path, hash) <- contents
                        ]
        files = [ (hash, init path, last path)
                | (hash, path) <- splittedIndex
                ]

        filesystem = foldl insertToInnerTree (InnerTree "" Map.empty) files

        trees = treesFromInnerTrees filesystem
