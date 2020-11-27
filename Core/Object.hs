module Core.Object where

import Data.List.Split
import qualified Codec.Compression.Zlib     as Zlib
import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map
import qualified System.Directory           as Dir

import Core.Core
import Core.Stage

data Object = Blob B.ByteString | Tree B.ByteString

data InnerTreeNode = InnerLeaf Hash
                   | InnerTree FilePath (Map.Map FilePath InnerTreeNode)

data HashedInnerTree = HashedInnerTree (Hash, Object, [HashedInnerTree])

objectContent :: Object -> B.ByteString
objectContent (Blob c) = c
objectContent (Tree c) = c

objectType :: Object -> B.ByteString
objectType (Blob _) = B.pack "blob"
objectType (Tree _) = B.pack "tree"

objectParse :: B.ByteString -> Object
objectParse bs = constructor objType content
  where objType = B.unpack $ (B.split ' ' bs) !! 0
        content = (B.split '\0' bs) !! 1
        constructor "blob" = Blob
        constructor "tree" = Tree
        constructor _ = Blob

objectUnparse :: Object -> B.ByteString
objectUnparse obj = B.concat $
  [ objType, B.pack " ", size, B.pack "\0"
  , content
  ]
  where objType = objectType obj
        content = objectContent obj
        size = B.pack $ show $ B.length content

hashObject :: Object -> Hash
hashObject = B16.encode . SHA1.hash . objectUnparse

hashString :: String -> Hash
hashString = hashObject . Blob . B.pack

compressObject :: Object -> B.ByteString
compressObject = L.toStrict . Zlib.compress . L.fromStrict . objectUnparse

decompressObject :: B.ByteString -> Object
decompressObject = objectParse . L.toStrict . Zlib.decompress . L.fromStrict

storeObject :: Object -> IO ()
storeObject obj = do
  Dir.createDirectoryIfMissing True completeDir
  B.writeFile path compressed
  where hashStr = B.unpack $ hashObject obj
        dir = take 2 $ hashStr
        filename = drop 2 $ hashStr
        completeDir = concat [".git/objects/", dir, "/"]
        path = completeDir ++ filename
        compressed = compressObject obj

loadObject :: B.ByteString -> IO Object
loadObject hash = B.readFile path >>= return . decompressObject
  where hashStr = B.unpack hash
        dir = take 2 $ hashStr
        filename = drop 2 $ hashStr
        path = concat [".git/objects/", dir, "/", filename]

treesFromStage :: Stage -> [Object]
treesFromStage stage = trees
  where splittedStage = [ (hash, splitOn "/" path)
                        | (hash, path) <- stage
                        ]

        files = [ (hash, init path, last path)
                | (hash, path) <- splittedStage
                ]

        filesystem = foldl insertToInnerTree (InnerTree "" Map.empty) files

        hashedInnerTrees = hashInnerTrees filesystem

        getTreeObjs (HashedInnerTree (_, parent, children)) = parent : (children >>= getTreeObjs)

        trees = hashedInnerTrees >>= getTreeObjs
        

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

hashInnerTrees :: InnerTreeNode -> [HashedInnerTree]
hashInnerTrees (InnerTree _ nodes) = [HashedInnerTree (B.pack "", Tree $ B.pack content, childTrees)]
  where nodeList = Map.toList nodes
        childTrees = nodeList >>= hashInnerTrees . snd
        blobLines = do
          (name, child) <- nodeList
          case child of (InnerTree _ _) -> []
                        (InnerLeaf hash) -> [
                          "100644" ++ " " ++
                          name ++ "\0" ++
                          (B.unpack . fst . B16.decode $ hash)
                           
                          ]
        hashedInnerTrees = do
          (name, child) <- nodeList
          [(name, c) | c <- hashInnerTrees child]

        treeLines = do
          (name, (HashedInnerTree (_, obj, _))) <- hashedInnerTrees
          [
            "040000" ++ " " ++
            name ++ "\0" ++
            (B.unpack . fst . B16.decode . hashObject $ obj)
            ]

        content = concat (blobLines ++ treeLines)

hashInnerTrees _ = []
  
