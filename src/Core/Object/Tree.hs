--------------------------------------------------------------------------------
-- |
-- Module      :  Core.Object.Blob
-- Copyright   :  (c) Lucas Oshiro 2022
--
-- Maintainer  : lucasseikioshiro@gmail.com
--
-- This modules contains the definition of the 'Tree' object as well as the
-- instance of the 'Object' typeclass.
--------------------------------------------------------------------------------

module Core.Object.Tree (Tree(..)) where

import Data.ByteString.Char8 (pack, unpack)
import Data.List (intercalate)
import Text.Read (readMaybe)

import qualified Data.ByteString.Base16 as B16
import qualified Data.ByteString.Char8  as B

import Core.Core (Hash, FileMode(..))
import Core.Object.Core (Object(..), ObjectType(..), rawObjectType)

newtype Tree = Tree [(FileMode, FilePath, Hash)]

instance Object Tree where
  objectType _ = TreeType

  objectParse raw = case objType of
    Just TreeType -> case parseTree raw of
                       (Just tree) -> return tree
                       Nothing     -> fail "Not a tree"
    _             -> fail "Invalid tree"
    where objType = rawObjectType raw

  objectRawContent (Tree children) = B.concat $
    [[ pack $ show mode, pack " "
     , pack name, pack "\0"
     , hash
     ]
    | (mode, name, hash) <- children
    ] >>= return . B.concat

  objectPretty (Tree entries) = intercalate "\n" $
    [ let objType = case filemode of DirMode -> "tree"
                                     StdMode -> "blob"
      in show filemode
      ++ " "
      ++ objType
      ++ " "
      ++ unpack hash
      ++ "    "
      ++ name
    | (filemode, name, hash) <- entries
    ]

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

            mode <- readMaybe . B.unpack $ mode'
            restEntries <- getEntries rest

            return $ (mode, name, hash):restEntries

  entries <- getEntries content
  return . Tree $ entries
