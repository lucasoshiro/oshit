--------------------------------------------------------------------------------
-- |
-- Module      :  Core.Object.Blob
-- Copyright   :  (c) Lucas Oshiro 2022
--
-- Maintainer  : lucasseikioshiro@gmail.com
--
-- This module contains the definition of the 'Commit' object, as well as the
-- instance of the 'Object' typeclass.
--------------------------------------------------------------------------------

module Core.Object.Commit (Commit(..)) where

import Data.ByteString.Char8 (pack, unpack)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.Time (ZonedTime, parseTimeM, defaultTimeLocale, formatTime)

import qualified Data.ByteString.Char8 as B

import Core.Core (Hash)
import Core.Object.Core (Object(..), ObjectType(..), rawObjectType, gitTimeFormat)

-- | A Git commit is a snapshot of the contents of the entire repository at a
-- given point in time.
data Commit = Commit
  { treeHash  :: Hash
  , parents   :: [Hash]
  , author    :: String
  , email     :: String
  , timestamp :: ZonedTime
  , message   :: String
  }

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
          parentsLines = (takeWhile $ B.isPrefixOf $ pack "parent") .
                         (drop 1) $
                         headerLines
          authorLine   = headerLines !! (length parentsLines + 1)

          tree       = B.drop (length "tree ") treeLine
          parents'   = [ B.drop (length "parent ") line
                       | line <- parentsLines
                       ]
          author'    = unpack .
                       B.init .
                       B.takeWhile (/= '<') .
                       B.drop (length "author ") $
                       authorLine
          email'     = unpack .
                       B.takeWhile (/= '>') .
                       B.drop 1 .
                       B.dropWhile (/= '<') $
                       authorLine
          timestampM :: Maybe ZonedTime
          timestampM = parseTimeM True defaultTimeLocale gitTimeFormat .
                       unpack .
                       B.drop 2 .
                       B.dropWhile (/= '>') $
                       authorLine
          timestamp' = fromJust timestampM
          message'   = unpack $
                       B.drop (1 + length headerLines + (sum $ map B.length headerLines)) $
                       content
          commit = Commit tree parents' author' email' timestamp' message'

  objectRawContent commit =
    pack . intercalate "\n" $
    [tree'] ++  parents' ++ [author', commiter', "", message commit]
    where tree'      = "tree" ++ " " ++ unpack (treeHash commit)
          parents'   = ["parent" ++ " " ++ unpack parent | parent <- parents commit]
          author'    = "author" ++ " " ++ author commit ++ " " ++ "<" ++ email commit ++ ">" ++ " " ++ timestamp'
          commiter'  = "commiter" ++ " " ++ author commit ++ " " ++ "<" ++ email commit ++ ">" ++ " " ++ timestamp'
          timestamp' = formatTime defaultTimeLocale gitTimeFormat (timestamp commit)

  objectPretty = unpack . objectRawContent
