module Core.Index where

-- Note: this index parser assumes index version 2

import Core.Core
import Core.Object
import Util.Util

import Data.Char
import Data.Either
import Data.Int
import Data.List
import Data.List.Split
import Foreign.C.Types
import System.PosixCompat.Files
import System.PosixCompat.Types hiding (FileMode)

import qualified Crypto.Hash.SHA1           as SHA1
import qualified Data.Binary                as Bin
import qualified Data.ByteString.Char8      as B
import qualified Data.ByteString.Base16     as B16
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Data.Map                   as Map
import qualified Data.Set                   as Set
import qualified System.Directory           as Dir


data IndexEntry = IndexEntry
  { ctime :: B.ByteString
  , mtime :: B.ByteString
  , dev   :: B.ByteString
  , ino   :: B.ByteString
  , mode  :: B.ByteString
  , uid   :: B.ByteString
  , gid   :: B.ByteString
  , size  :: B.ByteString
  , hash  :: B.ByteString
  , flags :: B.ByteString
  }

type Index = Map.Map B.ByteString IndexEntry

indexEntryStructure :: [Int]
indexEntryStructure =
  [ 8  -- ctime
  , 8  -- mtime
  , 4  -- dev
  , 4  -- ino
  , 4  -- mode
  , 4  -- uid
  , 4  -- gid
  , 4  -- size
  , 20 -- hash
  , 2  -- flags
  ]

indexPath :: FilePath
indexPath = ".git/index"

emptyIndexEntry :: IndexEntry
emptyIndexEntry = snd .
                  parseIndexEntry .
                  B.pack .
                  take (sum indexEntryStructure) .
                  repeat $
                  '\0'

parseIndexEntries :: Int -> [B.ByteString] -> [(B.ByteString, IndexEntry)]
parseIndexEntries 0 _ = []
parseIndexEntries _ [] = []
parseIndexEntries n (raw:rest) = parseIndexEntry raw :
                                 (parseIndexEntries (n - 1) rest)

parseIndexEntry :: B.ByteString -> (B.ByteString, IndexEntry)
parseIndexEntry raw =
  ( B.takeWhile (/= '\0') $ sliced !! 10
  , IndexEntry
    { ctime = sliced !! 0
    , mtime = sliced !! 1
    , dev   = sliced !! 2
    , ino   = sliced !! 3
    , mode  = sliced !! 4
    , uid   = sliced !! 5
    , gid   = sliced !! 6
    , size  = sliced !! 7
    , hash  = sliced !! 8
    , flags = sliced !! 9
    }
  )
  where sliced = sliceByteString indexEntryStructure raw

splitRawEntries :: B.ByteString -> [B.ByteString]
splitRawEntries bs = if B.length bs ==  0
                     then []
                     else next : splitRawEntries rest
  where next  = B.pack .
                (++ "\0") .
                map snd .
                takeWhile (\(i, c) -> (i < 62)
                            || (c /= '\0')
                            || (i `mod` 8 /= 7)
                          ) .
                zip ([0..] :: [Int]) $
                B.unpack bs
        rest = B.drop (B.length next) bs

parseIndex :: B.ByteString -> Index
parseIndex index =
  Map.fromList .
  parseIndexEntries (fromIntegral size) .
  splitRawEntries .
  (B.drop 12) $  -- skip header
  index
  where size = Bin.decode . L.fromStrict . B.take 4 . B.drop 8 $ index :: Int32

dumpIndex :: B.ByteString -> IO ()
dumpIndex index = putStrLn . intercalate "\n" . map (B.unpack . B16.encode) $
                  rawEntries
  where rawEntries = splitRawEntries .
                     B.reverse .
                     (B.drop 40) . -- drop sha-1
                     B.reverse .
                     (B.drop 12) $  -- drop header
                     index

readIndex :: IO Index
readIndex = Dir.doesFileExist indexPath >>= \p -> if p
  then B.readFile indexPath >>= return . parseIndex
  else return Map.empty

prettyIndex :: Index -> String
prettyIndex index = intercalate "\n" .
                    map (B.unpack . prettyEntry) .
                    sortByPath .
                    Map.toList $
                    index
  where prettyEntry :: (B.ByteString, IndexEntry) -> B.ByteString
        prettyEntry ( path
                    , (IndexEntry
                     { hash = hash
                     , mode = mode
                     })) = B.concat $
                          [(B16.encode mode)
                          , B.pack " "
                          , (B16.encode hash)
                          , B.pack " "
                          , path
                          ]
        sortByPath = sortBy (\a b -> compare (fst a) (fst b))

showIndex :: Index -> IO ()
showIndex index = putStrLn . prettyIndex $ index

unparseIndexEntry :: (B.ByteString, IndexEntry) -> B.ByteString
unparseIndexEntry (path, (IndexEntry c m d i mode u g size hash flags)) =
  content `B.append` trailing
  where content = B.concat [c, m, d, i, mode, u, g, size, hash, flags, path, B.pack "\0"]
        padding = (8 - (B.length content `mod` 8)) `mod` 8
        trailing = B.pack . take padding . repeat $ '\0'

unparseIndex :: Index -> B.ByteString
unparseIndex index = content `B.append` hash
  where header = B.concat [dirc, version, size]
        dirc = B.pack "DIRC"
        version = B.pack . map chr $ [0, 0, 0, 2]
        size = L.toStrict . Bin.encode $ (fromIntegral . length $ index :: Int32)
        body = B.concat . map unparseIndexEntry . Map.toList $ index
        content = header `B.append` body
        hash = SHA1.hash $ content

storeIndex :: Index -> IO ()
storeIndex = B.writeFile indexPath . unparseIndex

addBlobToIndex :: FilePath -> Hash -> Index -> Index
addBlobToIndex path hash' index = Map.insert (B.pack path) entry index
  where entry = emptyIndexEntry {hash = hash, flags = flags, mode = mode}
        pathSize = length path
        flags' :: Int16
        flags' = if pathSize <= 0x0FFF
                 then fromIntegral pathSize
                 else 0x0FFF
        flags = L.toStrict . Bin.encode $ flags'
        hash = fromRight B.empty . B16.decode $ hash'
        mode' = 100644 :: Int32
        mode = L.toStrict . Bin.encode $ mode'

addFileToIndex :: FilePath -> Index -> IO Index
addFileToIndex path index = do
  status <- getFileStatus path
  content <- B.readFile path
  let blob = Blob content
  let hash = hashObject blob
  let pathSize = length path
  let flags' = (if pathSize <= 0x0FFF
                then fromIntegral pathSize
                else 0x0FFF) :: Int16
  let flags = L.toStrict . Bin.encode $ flags'

  let entry = IndexEntry
        { ctime = (\(CTime c) -> toBS64 c) $ statusChangeTime status
        , mtime = (\(CTime c) -> toBS64 c) $ modificationTime status
        , dev   = (\(CDev  c) -> toBS32 c) $ deviceID         status
        , ino   = (\(CIno  c) -> toBS32 c) $ fileID           status
        , mode  = (\(CMode c) -> toBS32 c) $ fileMode         status
        , uid   = (\(CUid  c) -> toBS32 c) $ fileOwner        status
        , gid   = (\(CGid  c) -> toBS32 c) $ fileGroup        status
        , size  = (\(COff  c) -> toBS32 c) $ fileSize         status
        , hash  = fromRight B.empty . B16.decode $ hash
        , flags = flags
        }

  (objectExists . B.unpack $ hash) >>= \e ->
    if e
    then return ()
    else storeObject blob

  return $ Map.insert (B.pack path) entry index

updateAndStoreIndex :: Index -> [FilePath] -> IO ()
updateAndStoreIndex index files = do
  let adds = map addFileToIndex files
  newIndex <- foldl (>>=) (return index) adds
  storeIndex newIndex

indexStatus :: IO ([FilePath], [FilePath], [FilePath])
indexStatus = do
  index <- readIndex
  workdir <- listDirectoryRecursive "."
  workdirStatus <- sequence $ map getFileStatus workdir

  let index' = Map.fromList $
               [ (B.unpack path', mtime)
               | (path', IndexEntry {mtime = mtime}) <- Map.toList index
               ]


  let workdir' = Map.fromList $
                 [ (path, (\(CTime c) -> toBS64 c) $ modificationTime status)
                 | (path, status) <- zip workdir workdirStatus
                 ]

  let indexFiles   = Map.keysSet index'
  let workdirFiles = Set.fromList workdir

  let deleted   = indexFiles   Set.\\ workdirFiles
  let untracked = workdirFiles Set.\\ indexFiles

  let joined = zipMap
               (index' `Map.withoutKeys` deleted)
               (workdir' `Map.withoutKeys` untracked)

  let touched = [ path
                | (path, (timeI, timeW)) <- Map.toList joined
                , (timeI /= timeW)
                ]

  let contentHash path = B.readFile path >>= return . hashObject . Blob

  modified <- (
        sequence $ do
            f <- touched
            [ do
                wHash <- contentHash f
                let iHash = (\(IndexEntry {hash = hash}) -> B16.encode hash) $
                            (index Map.! (B.pack f))
                if wHash == iHash
                  then return [f]
                  else return []
              ]
        ) >>= return . (>>= id)

  return (modified, sort . Set.toList $ deleted, sort . Set.toList $ untracked)

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

indexFromTree :: Tree -> IO Index
indexFromTree (Tree entries) = indexFromTree' [] entries

indexFromTree' :: [String] -> [(FileMode, FilePath, Hash)] -> IO Index
indexFromTree' _ [] = return $ Map.empty
indexFromTree' path ((entryMode, name, entryHash):rest) =
  do
    let fullPath = path ++ [name]
    let rawFullPath = intercalate "/" fullPath
    let entryModeInt = parseOctal entryMode :: Int

    let subIndex
          | entryModeInt == parseOctal dirMode =
            loadObject entryHash >>= \(Tree entries) -> indexFromTree' fullPath entries
          | entryModeInt == parseOctal standardMode =
            return $ addBlobToIndex rawFullPath entryHash Map.empty
          | otherwise =
            return Map.empty

    it <- subIndex
    r <- indexFromTree' path rest
    return $ r `Map.union` it
