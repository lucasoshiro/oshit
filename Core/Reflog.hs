module Core.Reflog where

import Data.List
import Data.List.Split
import Data.Maybe
import Data.Time

import qualified Data.ByteString.Char8 as B

import Core.Core
import Core.Object (gitTimeFormat)
import Core.Reference (branchExists)

data ReflogEntry = ReflogEntry
  { oldHash   :: Hash
  , newHash   :: Hash
  , author    :: String
  , email     :: String
  , timestamp :: ZonedTime
  , title     :: String
  }

type Reflog = [ReflogEntry]

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

readReflogFile :: String -> IO Reflog
readReflogFile ref = do
  let reflogBasePath = ".git/logs/"
  isBranch <- branchExists ref

  let isStash = ref == "stash"

  let path = if isBranch
             then reflogBasePath ++ "refs/heads/" ++ ref
             else if isStash
                  then "refs/stash"
                  else reflogBasePath ++ ref

  content <- readFile path
  return . parseReflog $ content
