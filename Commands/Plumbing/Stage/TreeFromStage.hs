module Commands.Plumbing.Stage.TreeFromStage where

import Core.Core
import Core.Object.Object
import Core.Object.Tree
import Core.Stage

import qualified Data.ByteString.Char8 as B

cmdTreeFromStage :: Command
cmdTreeFromStage _ = do
  stage <- loadStage
  let objs = treesFromStage stage
  let rootHash = hashObject $ head objs
  let objsIOs = map storeObject objs
  let outputHash = putStrLn $ B.unpack rootHash
  sequence_ $ objsIOs ++ [outputHash]

