module Core.Object.Commit where

import Data.List
import Data.Maybe
import Data.Time
import qualified Data.ByteString.Char8 as B

import Core.Core
import Core.Object.Object

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

