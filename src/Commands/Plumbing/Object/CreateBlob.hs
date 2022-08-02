module Commands.Plumbing.Object.CreateBlob where

import Core.Core
import Core.Object

import qualified Data.ByteString.Char8 as B

cmdCreateBlob :: Command
cmdCreateBlob args = do
  let readContent = if length args > 0 then B.readFile $ args !! 0 else B.getContents
  content <- readContent
  let obj = Blob content
  putStrLn . B.unpack $hashObject obj
  storeObject obj
