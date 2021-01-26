module Core.Object.Tree where

import Data.Either
import Data.List
import Data.List.Split
import qualified Data.ByteString.Char8      as B
import qualified Data.Map                   as Map
import qualified Data.ByteString.Base16     as B16

import Core.Core
import Core.Index
import Core.Object.Object

data InnerTreeNode = InnerLeaf Hash
                   | InnerTree FilePath (Map.Map FilePath InnerTreeNode)

data HashedInnerTree = HashedInnerTree (Hash, Tree, [HashedInnerTree])

instance Object Tree where
  objectType _ = B.pack "tree"

  objectParse = return . parseTree

  objectRawContent (Tree children) = B.concat $
    [[ B.pack mode, B.pack " "
     , B.pack name, B.pack "\0"
     , hash
     ]
    | (mode, name, hash) <- children
    ] >>= return . B.concat

  objectPretty (Tree entries) = intercalate "\n" $
    [ let objType = case filemode of "040000" -> "tree"
                                     "100644" -> "blob"
                                     _ -> ""
      in filemode ++ " " ++ objType ++ " " ++ B.unpack hash ++ "    " ++ name
    | (filemode, name, hash) <- entries
    ]

treesFromIndex :: Index -> [Tree]
treesFromIndex index = trees
  where splittedIndex = [ (B16.encode hash, splitOn "/" . B.unpack $ path)
                        | (path, (IndexEntry {hash = hash})) <- Map.toList index
                        ]
        files = [ (hash, init path, last path)
                | (hash, path) <- splittedIndex
                ]

        filesystem = foldl insertToInnerTree (InnerTree "" Map.empty) files

        trees = treesFromInnerTrees filesystem


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
                          ("100644", name, (fromRight B.empty . B16.decode $ hash))
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
          [ ("040000", name, (fromRight B.empty . B16.decode . hashObject $ tree)) ]

        content = blobEntries ++ treeEntries
treesFromInnerTrees _ = []

parseTree :: B.ByteString -> Tree
parseTree raw = Tree . getEntries $ content
  where content = (B.drop 1) . (B.dropWhile (/= '\0')) $ raw

        getEntries :: B.ByteString -> [(FileMode, FilePath, Hash)]
        getEntries bs
          | bs == B.empty = []
          | otherwise     = (mode, name, hash):(getEntries rest)
          where (mode', rest')   = B.splitAt 6 $ bs
                (name',  rest'') = B.span (/= '\0') $ B.drop 1 rest'
                (hash', rest)    = B.splitAt 40 $ B.drop 1 rest''

                mode = B.unpack mode'
                name = B.unpack name'
                hash = B16.encode hash'

listTreeRecursive :: Tree -> IO [FilePath]
listTreeRecursive = flip listTreeRecursive' ""

listTreeRecursive' :: Tree -> FilePath -> IO [FilePath]
listTreeRecursive' (Tree entries) path = do
  let x :: IO [[FilePath]]
      x = sequence $ do
        (mode, name, hash) <- entries
        let childPath = path ++ name :: FilePath
        return $ do
          childTree <- loadObject hash :: IO Tree
          if mode == dirMode
            then listTreeRecursive' childTree (childPath ++ "/") >>= return . (childPath :)
            else return [childPath]
            
  x >>= return . (>>= id)

