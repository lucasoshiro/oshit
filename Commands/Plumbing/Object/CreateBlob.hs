module Commands.Plumbing.Object.CreateBlob where

import Core.Object
import Core.Core

import qualified Data.ByteString.Char8 as B

createBlob :: Command
createBlob args = do
  let readContent = if length args > 0 then B.readFile $ args !! 0 else B.getContents
  content <- readContent
  let obj = Blob content
  storeObject obj
