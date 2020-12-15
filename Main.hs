import Core.Core

import Commands.Plumbing.Object.CreateBlob
import Commands.Plumbing.Object.ShowObj
import Commands.Plumbing.Stage.StageBlob
import Commands.Plumbing.Stage.TreeFromStage
import Commands.Plumbing.Reference.UpdateBranch
import Commands.Plumbing.Reference.UpdateHead
import Commands.Plumbing.Commit.CommitFromTree

import Commands.Porcelain.Init
import Commands.Porcelain.Stage.StageStatus

import System.Environment


runCmd :: String -> Command
runCmd "init"             = cmdInit
runCmd "create-blob"      = cmdCreateBlob
runCmd "show-obj"         = cmdShowObj
runCmd "stage-blob"       = cmdStageBlob
runCmd "stage-status"     = cmdStageStatus
runCmd "tree-from-stage"  = cmdTreeFromStage
runCmd "commit-from-tree" = cmdCommitFromTree
runCmd "update-branch"    = cmdUpdateBranch
runCmd "update-head"      = cmdUpdateHead
runCmd _ = \_ -> fail "invalid command"

main :: IO ()
main = do
  cliArgs <- getArgs
  let cmd = head cliArgs
  let args = tail cliArgs
  runCmd cmd args
