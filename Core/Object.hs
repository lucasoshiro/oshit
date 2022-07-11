module Core.Object where

import Data.Either
import Data.List
import Data.Maybe
import Data.Time

import qualified Codec.Compression.Zlib     as Zlib
import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map
import qualified System.Directory           as Dir

import Core.Core
import Util.Util

type ObjectType = B.ByteString

data Tree = Tree [(FileMode, FilePath, Hash)]
data Blob = Blob B.ByteString
data Commit = Commit
  { treeHash  :: Hash
  , parents   :: [Hash]
  , author    :: String
  , email     :: String
  , timestamp :: ZonedTime
  , message   :: String
  }

class Object obj where
  objectType       :: obj -> ObjectType
  objectParse      :: B.ByteString -> IO obj
  objectRawContent :: obj -> B.ByteString
  objectPretty     :: obj -> String

data InnerTreeNode = InnerLeaf Hash
                   | InnerTree FilePath (Map.Map FilePath InnerTreeNode)

data HashedInnerTree = HashedInnerTree (Hash, Tree, [HashedInnerTree])


hashObject :: Object obj => obj -> Hash
hashObject = B16.encode . SHA1.hash . objectFileContent

compress :: B.ByteString -> B.ByteString
compress = L.toStrict . Zlib.compress . L.fromStrict

decompress :: B.ByteString -> B.ByteString
decompress = L.toStrict . Zlib.decompress . L.fromStrict

storeObject :: Object obj => obj -> IO ()
storeObject obj = do
  Dir.createDirectoryIfMissing True completeDir
  B.writeFile path compressed
  where hashStr      = B.unpack $ hashObject obj
        dir          = take 2 $ hashStr
        filename     = drop 2 $ hashStr
        completeDir  = concat [".git/objects/", dir, "/"]
        path         = completeDir ++ filename
        uncompressed = objectFileContent obj
        compressed   = compress uncompressed

loadObject :: Object obj => Hash -> IO obj
loadObject hash = loadRawObject hash >>= objectParse

loadRawObject :: Hash -> IO B.ByteString
loadRawObject hash = B.readFile (hashPath hash) >>= return . decompress

objectFileContent :: Object obj => obj -> B.ByteString
objectFileContent obj = uncompressed
  where content      = objectRawContent obj
        size         = B.pack $ show $ B.length content
        objType      = objectType obj
        uncompressed = B.concat [objType, B.pack " ", size, B.pack "\0", content]

rawObjectType :: B.ByteString -> ObjectType
rawObjectType = B.takeWhile (/= ' ')

hashPath :: Hash -> FilePath
hashPath hash = path
  where hashStr  = B.unpack hash
        dir      = take 2 $ hashStr
        filename = drop 2 $ hashStr
        path     = concat [".git/objects/", dir, "/", filename]

objectExists :: String -> IO Bool
objectExists hashStr = do
  let path = ".git/objects/" ++ take 2 hashStr ++ "/" ++ drop 2 hashStr
  Dir.doesFileExist path

-- Blob

instance Object Blob where
  objectParse bs =
    if parsedType == "blob"
    then return $ Blob content
    else fail ""
    where parsedType = B.unpack $ (B.split ' ' bs) !! 0
          content = (B.split '\0' bs) !! 1

  objectType _ = B.pack "blob"

  objectRawContent (Blob content) = content

  objectPretty (Blob bs) = B.unpack bs

-- Commit

gitTimeFormat = "%s %z"

instance Object Commit where
  objectType _ = B.pack "commit"

  objectParse raw = return commit
    where content      = (B.drop 1) . (B.dropWhile (/= '\0')) $ raw
          lines        = B.split '\n' content
          headerLines  = takeWhile (/= B.empty) lines
          treeLine     = headerLines !! 0
          parentsLines = (takeWhile $ B.isPrefixOf $ B.pack "parent") .
                         (drop 1) $
                         headerLines
          authorLine   = headerLines !! (length parentsLines + 1)

          tree       = B.drop (length "tree ") treeLine
          parents    = [ B.drop (length "parent ") line
                       | line <- parentsLines
                       ]
          author     = B.unpack .
                       B.init .
                       B.takeWhile (/= '<') .
                       B.drop (length "author ") $
                       authorLine
          email      = B.unpack .
                       B.takeWhile (/= '>') .
                       B.drop 1 .
                       B.dropWhile (/= '<') $
                       authorLine
          timestamp' :: Maybe ZonedTime
          timestamp' = parseTimeM True defaultTimeLocale gitTimeFormat .
                       B.unpack .
                       B.drop 2 .
                       B.dropWhile (/= '>') $
                       authorLine
          timestamp  = fromJust timestamp'
          message    = B.unpack $
                       B.drop (1 + length headerLines + (sum $ map B.length headerLines)) $
                       content
          commit = Commit tree parents author email timestamp message

  objectRawContent (Commit treeHash parents author email timestamp message) =
    B.pack . intercalate "\n" $
    [tree'] ++  parents' ++ [author', commiter', "", message]
    where tree'      = "tree" ++ " " ++ B.unpack treeHash
          parents'   = ["parent" ++ " " ++ B.unpack parent | parent <- parents]
          author'    = "author" ++ " " ++ author ++ " " ++ "<" ++ email ++ ">" ++ " " ++ timestamp'
          commiter'  = "commiter" ++ " " ++ author ++ " " ++ "<" ++ email ++ ">" ++ " " ++ timestamp'
          timestamp' = formatTime defaultTimeLocale gitTimeFormat timestamp

  objectPretty = B.unpack . objectRawContent


instance Object Tree where
  objectType _ = B.pack "tree"

  objectParse raw = case parseTree raw of
                      (Just tree) -> return tree
                      Nothing     -> fail "Invalid tree"

  objectRawContent (Tree children) = B.concat $
    [[ B.pack . stringFromFileMode $ mode, B.pack " "
     , B.pack name, B.pack "\0"
     , hash
     ]
    | (mode, name, hash) <- children
    ] >>= return . B.concat

  objectPretty (Tree entries) = intercalate "\n" $
    [ let objType = case filemode of DirMode -> "tree"
                                     StdMode -> "blob"
      in stringFromFileMode filemode
      ++ " "
      ++ objType
      ++ " "
      ++ B.unpack hash
      ++ "    "
      ++ name
    | (filemode, name, hash) <- entries
    ]

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

parseTree :: B.ByteString -> Maybe Tree
parseTree raw = do
  let content = (B.drop 1) . (B.dropWhile (/= '\0')) $ raw
  let getEntries bs
        | bs == B.empty = Just []
        | otherwise     = do
            let (mode', rest')  = B.break (== ' ') bs
            let (name', rest'') = B.span (/= '\0') $ B.drop 1 rest'
            let (hash', rest)   = B.splitAt 20 $ B.drop 1 rest''

            let name = B.unpack name'
            let hash = B16.encode hash'

            mode <- fileModeFromString . B.unpack $ mode'
            restEntries <- getEntries rest

            return $ (mode, name, hash):restEntries

  entries <- getEntries content
  return . Tree $ entries

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
          case mode of
             DirMode -> listTreeRecursive' childTree (childPath ++ "/") >>= return . (childPath :)
             StdMode -> return [childPath]

  x >>= return . (>>= id)
