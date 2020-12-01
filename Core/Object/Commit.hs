module Core.Object.Commit where

import Data.List
import Data.Time
import qualified Data.ByteString.Char8      as B

import Core.Core
import Core.Object.Object

instance Object Commit where
  objectType _ = B.pack "commit"

  objectParse commit = fail "not implemented"

  objectRawContent (Commit treeHash parents author email timestamp message) =
    B.pack . intercalate "\n" $
    [tree'] ++  parents' ++ [author', commiter', "", message]
    where tree'      = "tree" ++ " " ++ B.unpack treeHash
          parents'   = ["parent" ++ " " ++ B.unpack parent | parent <- parents]
          author'    = "author" ++ " " ++ author ++ " " ++ "<" ++ email ++ ">" ++ " " ++ timestamp'
          commiter'  = "commiter" ++ " " ++ author ++ " " ++ "<" ++ email ++ ">" ++ " " ++ timestamp'
          timestamp' = formatTime defaultTimeLocale "%s %z" timestamp

  objectPretty = B.unpack . objectRawContent
