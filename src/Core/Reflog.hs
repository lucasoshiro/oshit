{-# LANGUAGE TupleSections #-}

module Core.Reflog where

import Data.List
import Data.List.Extra (trim)
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

instance Read ReflogEntry where
  readsPrec _ s = [(parsedEntry, "")]
    where
      metadata:t:_ = splitOn "\t" s
      ohash:nhash:_    = map B.pack . take 2 . splitOn " " $ metadata
      aut:mail:tsstr:_ = map trim . splitOneOf "<>" $ drop 82 metadata -- hashes and whitespace
      tstamp = fromJust . parseTimeM True defaultTimeLocale gitTimeFormat $ tsstr
      parsedEntry = ReflogEntry ohash nhash aut mail tstamp t

  readList = pure . (, "") . map read . filter (not . null) . splitOn "\n"

reflogBasePath :: String
reflogBasePath = ".git/logs/"

readReflog :: FilePath -> IO Reflog
readReflog path = read <$> readFile path

readSymrefReflog :: String -> IO Reflog
readSymrefReflog ref = readReflog $ reflogBasePath ++ ref

readStashReflog :: IO Reflog
readStashReflog = readReflog $ reflogBasePath ++ "refs/stash"

readBranchReflog :: String -> IO Reflog
readBranchReflog ref = readReflog $ reflogBasePath ++ "refs/heads/" ++ ref
