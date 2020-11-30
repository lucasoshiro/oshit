module Commands.Plumbing.Stage.StageBlob where

import qualified Data.ByteString.Char8 as B

import Core.Core
import Core.Object.Blob
import Core.Object.Object
import Core.Stage

cmdStageBlob :: Command
cmdStageBlob (hash:filepath:_) = do
  let hashBytes = B.pack hash
  obj <- loadObject hashBytes :: IO Blob
  let objType = objectType obj
  if objType /= B.pack "blob" then fail "" else return ()
  old <- loadStage
  let new = appendBlob old hashBytes filepath
  storeStage new
cmdStageBlob (_:_) = fail "filepath missing"
cmdStageBlob _ = fail "hash missing"
