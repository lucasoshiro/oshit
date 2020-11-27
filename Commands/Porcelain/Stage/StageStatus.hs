module Commands.Porcelain.Stage.StageStatus where

import Core.Core
import Core.Stage

import Data.List
import qualified Data.ByteString.Char8 as B

cmdStageStatus :: Command
cmdStageStatus _ = do
  stage <- loadStage
  let output = intercalate "\n" $ [ B.unpack hash ++ " " ++ name
                                  | (hash, name) <- stage]
  putStrLn output
