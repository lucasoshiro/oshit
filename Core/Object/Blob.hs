module Core.Object.Blob where

import qualified Data.ByteString.Char8      as B

import Core.Object.Object

data Blob = Blob B.ByteString

instance Object Blob where
  objectParse bs =
    if parsedType == "blob"
    then return $ Blob content
    else fail ""
    where parsedType = B.unpack $ (B.split ' ' bs) !! 0
          content = (B.split '\0' bs) !! 1
  objectType _ = B.pack "blob"
  objectRawContent (Blob content) = content
