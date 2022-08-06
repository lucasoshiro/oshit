module Core.Object where

import Data.Either
import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time

import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Char8      as B
import qualified Data.Map                   as Map
import qualified System.Directory           as Dir

import Core.Core
import Core.Packfile
import Util.Compression

data ObjectType = BlobType | TreeType | CommitType

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

type BlobIO   = IO Blob
type TreeIO   = IO Tree
type CommitIO = IO Commit

data ObjectIO = ObjectIO BlobIO TreeIO CommitIO

data InnerTreeNode = InnerLeaf Hash
                   | InnerTree FilePath (Map.Map FilePath InnerTreeNode)

data HashedInnerTree = HashedInnerTree (Hash, Tree, [HashedInnerTree])

parseObjectType :: String -> Maybe ObjectType
parseObjectType "blob"   = Just BlobType
parseObjectType "tree"   = Just TreeType
parseObjectType "commit" = Just CommitType
parseObjectType _        = Nothing

unparseObjectType :: ObjectType -> String
unparseObjectType BlobType   = "blob"
unparseObjectType TreeType   = "tree"
unparseObjectType CommitType = "commit"

hashObject :: Object obj => obj -> Hash
hashObject = B16.encode . SHA1.hash . objectFileContent

storeObject :: Object obj => obj -> IO ()
storeObject obj = do
  Dir.createDirectoryIfMissing True completeDir
  -- TODO: packfile
  exists <- looseObjectExists hashStr

  if exists
    then return ()
    else B.writeFile path compressed

  where hashStr      = B.unpack $ hashObject obj
        dir          = take 2 $ hashStr
        filename     = drop 2 $ hashStr
        completeDir  = concat [".git/objects/", dir, "/"]
        path         = completeDir ++ filename
        uncompressed = objectFileContent obj
        compressed   = compress uncompressed

loadObjectLegacy :: Object obj => Hash -> IO obj
loadObjectLegacy hash = loadRawObject hash >>= objectParse

loadLooseRawObject :: Hash -> IO B.ByteString
loadLooseRawObject hash = B.readFile (hashPath hash) >>= return . decompress

loadPackedRawObject :: Hash -> IO (Maybe B.ByteString)
loadPackedRawObject hash = do
  obj <- searchInPackFiles hash
  return $ do
    (objType, _, content) <- obj

    typeStr <- case objType of
                    PackBlob   -> Just . B.pack $ "blob"
                    PackTree   -> Just . B.pack $ "tree"
                    PackCommit -> Just . B.pack $ "commit"
                    _          -> Nothing

    let size = B.length content

    return . B.concat $ [ typeStr
                        , B.pack " "
                        , B.pack . show $ size
                        , B.pack "\0"
                        , content
                        ]

loadRawObject :: Hash -> IO B.ByteString
loadRawObject hash = do
  exists <- looseObjectExists . B.unpack $ hash
  if exists
    then loadLooseRawObject $ hash
    else do
      packed <- loadPackedRawObject hash

      case packed of
        Just b -> return b
        Nothing -> fail "object not found"

loadObject :: Hash -> ObjectIO
loadObject hash = ObjectIO blobIO treeIO commitIO
  where raw      = loadRawObject hash
        blobIO   = raw >>= objectParse
        treeIO   = raw >>= objectParse
        commitIO = raw >>= objectParse

loadBlob :: Hash -> BlobIO
loadBlob hash = blobIO
  where (ObjectIO blobIO _ _) = loadObject hash
        
loadTree :: Hash -> TreeIO
loadTree hash = treeIO
  where (ObjectIO _ treeIO _) = loadObject hash

loadCommit :: Hash -> CommitIO
loadCommit hash = commitIO
  where (ObjectIO _ _ commitIO) = loadObject hash

objectFileContent :: Object obj => obj -> B.ByteString
objectFileContent obj = uncompressed
  where content      = objectRawContent obj
        size         = B.pack $ show $ B.length content
        objType      = objectType obj
        uncompressed = B.concat [ B.pack . unparseObjectType $ objType
                                , B.pack " "
                                , size
                                , B.pack "\0", content]

rawObjectType :: B.ByteString -> Maybe ObjectType
rawObjectType = parseObjectType . B.unpack . (B.takeWhile (/= ' '))

hashPath :: Hash -> FilePath
hashPath hash = path
  where hashStr  = B.unpack hash
        dir      = take 2 $ hashStr
        filename = drop 2 $ hashStr
        path     = concat [".git/objects/", dir, "/", filename]

looseObjectExists :: String -> IO Bool
looseObjectExists hashStr = do
  let path = ".git/objects/" ++ take 2 hashStr ++ "/" ++ drop 2 hashStr
  Dir.doesFileExist path

-- Blob

instance Object Blob where
  objectParse bs = case objType of
    Just BlobType -> return . Blob $ content
    _             -> fail "Not a blob"
    where objType = rawObjectType bs
          content = (B.split '\0' bs) !! 1

  objectType _ = BlobType

  objectRawContent (Blob content) = content

  objectPretty (Blob bs) = B.unpack bs

-- Commit

gitTimeFormat :: String
gitTimeFormat = "%s %z"

instance Object Commit where
  objectType _ = CommitType

  objectParse raw =
    case objType of
      Just CommitType -> return commit
      _           -> fail "Not a commit"

    where objType      = rawObjectType raw
          content      = (B.drop 1) . (B.dropWhile (/= '\0')) $ raw
          linesB       = B.split '\n' content
          headerLines  = takeWhile (/= B.empty) linesB
          treeLine     = headerLines !! 0
          parentsLines = (takeWhile $ B.isPrefixOf $ B.pack "parent") .
                         (drop 1) $
                         headerLines
          authorLine   = headerLines !! (length parentsLines + 1)

          tree       = B.drop (length "tree ") treeLine
          parents'   = [ B.drop (length "parent ") line
                       | line <- parentsLines
                       ]
          author'    = B.unpack .
                       B.init .
                       B.takeWhile (/= '<') .
                       B.drop (length "author ") $
                       authorLine
          email'     = B.unpack .
                       B.takeWhile (/= '>') .
                       B.drop 1 .
                       B.dropWhile (/= '<') $
                       authorLine
          timestampM :: Maybe ZonedTime
          timestampM = parseTimeM True defaultTimeLocale gitTimeFormat .
                       B.unpack .
                       B.drop 2 .
                       B.dropWhile (/= '>') $
                       authorLine
          timestamp' = fromJust timestampM
          message'   = B.unpack $
                       B.drop (1 + length headerLines + (sum $ map B.length headerLines)) $
                       content
          commit = Commit tree parents' author' email' timestamp' message'

  objectRawContent commit =
    B.pack . intercalate "\n" $
    [tree'] ++  parents' ++ [author', commiter', "", message commit]
    where tree'      = "tree" ++ " " ++ B.unpack (treeHash commit)
          parents'   = ["parent" ++ " " ++ B.unpack parent | parent <- parents commit]
          author'    = "author" ++ " " ++ author commit ++ " " ++ "<" ++ email commit ++ ">" ++ " " ++ timestamp'
          commiter'  = "commiter" ++ " " ++ author commit ++ " " ++ "<" ++ email commit ++ ">" ++ " " ++ timestamp'
          timestamp' = formatTime defaultTimeLocale gitTimeFormat (timestamp commit)

  objectPretty = B.unpack . objectRawContent


-- Tree

instance Object Tree where
  objectType _ = TreeType

  objectParse raw = case objType of
    Just TreeType -> case parseTree raw of
                       (Just tree) -> return tree
                       Nothing     -> fail "Not a tree"
    _             -> fail "Invalid tree"
    where objType = rawObjectType raw

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
