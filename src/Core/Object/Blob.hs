--------------------------------------------------------------------------------
-- |
-- Module      :  Core.Object.Blob
-- Copyright   :  (c) Lucas Oshiro 2022
--
-- Maintainer  : lucasseikioshiro@gmail.com
--
-- This module contains the definition of the 'Blob' object, as well as the
-- instance of the 'Object' typeclass.
--------------------------------------------------------------------------------

module Core.Object.Blob (
    Blob(..)
  ) where

import Data.ByteString.Char8 (ByteString, unpack, split)

import Core.Object.Core (Object(..), ObjectType(..), rawObjectType)

-- | A Git blob is a file in the Git object database. It is a sequence of bytes.
newtype Blob = Blob ByteString deriving (Show, Eq)

instance Object Blob where
  objectParse bs = case objType of
    Just BlobType -> return . Blob $ content
    _             -> fail "Not a blob"
    where objType = rawObjectType bs
          content = split '\0' bs !! 1

  objectType _ = BlobType

  objectRawContent (Blob content) = content

  objectPretty (Blob bs) = unpack bs
