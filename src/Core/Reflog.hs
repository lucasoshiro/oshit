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

instance Show ReflogEntry where
  show entry = intercalate "\t"
    [ unwords [ oldHash', newHash', author', timestamp' ]
    , title entry
    ]
    where oldHash'   = B.unpack (oldHash entry)
          newHash'   = B.unpack (newHash entry)
          timestamp' = formatTime defaultTimeLocale gitTimeFormat (timestamp entry)
          author'    = author entry ++ " " ++ "<" ++ email entry ++ ">"

  showsPrec = const $ (++) . show
  showList  = (++) . intercalate "\n" . map show

reflogBasePath :: String
reflogBasePath = ".git/logs/"

parseReflog :: String -> Reflog
parseReflog s = map parseReflogEntry . init $ splitOn "\n" s

parseReflogEntry :: String -> ReflogEntry
parseReflogEntry s = ReflogEntry
  { oldHash   = parsedOldHash
  , newHash   = parsedNewHash
  , author    = parsedAuthor
  , email     = parsedEmail
  , timestamp = parsedTimestamp
  , title     = parsedTitle
  }
  where s':parsedTitle:_ = splitOn "\t" s
        oldHash':newHash':_ = take 2 . splitOn " " $ s'

        parsedOldHash = B.pack oldHash'
        parsedNewHash = B.pack newHash'

        parsedAuthorLine = drop 82 s' -- drop heading hashes and spaces

        parsedAuthor = init .
                       takeWhile (/= '<') $
                       parsedAuthorLine

        parsedEmail = takeWhile (/= '>') .
                      drop 1 .
                      dropWhile (/= '<') $
                      parsedAuthorLine

        parsedTimestamp :: ZonedTime
        parsedTimestamp = fromJust .
                          parseTimeM True defaultTimeLocale gitTimeFormat .
                          drop 2 .
                          dropWhile (/= '>') $
                          parsedAuthorLine

readReflog :: FilePath -> IO Reflog
readReflog path = readFile path >>= return . parseReflog
  
readSymrefReflog :: String -> IO Reflog
readSymrefReflog ref = readReflog $ reflogBasePath ++ ref

readStashReflog :: IO Reflog
readStashReflog = readReflog $ reflogBasePath ++ "refs/stash"

readBranchReflog :: String -> IO Reflog
readBranchReflog ref = readReflog $ reflogBasePath ++ "refs/heads/" ++ ref
