module Core.Reflog where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time

import qualified Data.ByteString.Char8 as B

import Core.Core
import Core.Object (gitTimeFormat)

data ReflogEntry = ReflogEntry
  { oldHash   :: Hash
  , newHash   :: Hash
  , author    :: String
  , email     :: String
  , timestamp :: ZonedTime
  , title     :: String
  }

type Reflog = [ReflogEntry]

reflogBasePath :: String
reflogBasePath = ".git/logs/"

reflogToString :: Reflog -> String
reflogToString reflog = intercalate "\n" $ map reflogEntryToString reflog

reflogEntryToString :: ReflogEntry -> String
reflogEntryToString
  (ReflogEntry { oldHash   = oldHash
               , newHash   = newHash
               , author    = author
               , email     = email
               , timestamp = timestamp
               , title     = title
               }) = intercalate "\t" $
                    [ (intercalate " " [oldHash', newHash', author', timestamp'])
                    , title
                    ]
  where oldHash'   = B.unpack oldHash
        newHash'   = B.unpack newHash
        timestamp' = formatTime defaultTimeLocale gitTimeFormat timestamp
        author'    = author ++ " " ++ "<" ++ email ++ ">"

parseReflog :: String -> Reflog
parseReflog s = map parseReflogEntry . init $ splitOn "\n" s

parseReflogEntry :: String -> ReflogEntry
parseReflogEntry s = ReflogEntry
  { oldHash   = oldHash
  , newHash   = newHash
  , author    = author
  , email     = email
  , timestamp = timestamp
  , title     = title
  }
  where s':title:_ = splitOn "\t" s
        oldHash':newHash':_ = take 2 . splitOn " " $ s'

        oldHash = B.pack oldHash'
        newHash = B.pack newHash'

        authorLine = drop 82 s' -- drop heading hashes and spaces

        author = init .
                 takeWhile (/= '<') .
                 drop 1 $
                 authorLine

        email = takeWhile (/= '>') .
                drop 1 .
                dropWhile (/= '<') $
                authorLine

        timestamp :: ZonedTime
        timestamp = fromJust .
                    parseTimeM True defaultTimeLocale gitTimeFormat .
                    drop 2 .
                    dropWhile (/= '>') $
                    authorLine

readReflog :: FilePath -> IO Reflog
readReflog path = readFile path >>= return . parseReflog
  
readSymrefReflog :: String -> IO Reflog
readSymrefReflog ref = readReflog $ reflogBasePath ++ ref

readStashReflog :: IO Reflog
readStashReflog = readReflog $ reflogBasePath ++ "refs/stash"

readBranchReflog :: String -> IO Reflog
readBranchReflog ref = readReflog $ reflogBasePath ++ "refs/heads/" ++ ref
