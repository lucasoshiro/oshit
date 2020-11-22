module Commands.Porcelain.Stage.StageStatus where

import Core.Core
import Core.Stage

import Data.List

cmdStageStatus :: Command
cmdStageStatus _ = do
  stage <- loadStage
  let output = intercalate "\n" $ map snd stage
  putStrLn output
