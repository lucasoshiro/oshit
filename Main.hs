import Core.Core

import Commands.Plumbing.Commit.CommitFromTree
import Commands.Plumbing.Index.ListIndex
import Commands.Plumbing.Index.TreeFromIndex
import Commands.Plumbing.Object.CreateBlob
import Commands.Plumbing.Object.ShowObj
import Commands.Plumbing.Reference.UpdateBranch
import Commands.Plumbing.Reference.UpdateHead
import Commands.Plumbing.Stage.StageBlob
import Commands.Plumbing.Stage.TreeFromStage

import Commands.Porcelain.Init
import Commands.Porcelain.Log
import Commands.Porcelain.Stage.StageStatus

import System.Environment


runCmd :: String -> Command
runCmd "commit-from-tree" = cmdCommitFromTree
runCmd "create-blob"      = cmdCreateBlob
runCmd "init"             = cmdInit
runCmd "log"              = cmdLog
runCmd "list-index"       = cmdListIndex
runCmd "show-obj"         = cmdShowObj
runCmd "stage-blob"       = cmdStageBlob
runCmd "stage-status"     = cmdStageStatus
runCmd "tree-from-index"  = cmdTreeFromIndex
runCmd "tree-from-stage"  = cmdTreeFromStage
runCmd "update-branch"    = cmdUpdateBranch
runCmd "update-head"      = cmdUpdateHead
runCmd _ = \_ -> fail "invalid command"

main :: IO ()
main = do
  cliArgs <- getArgs
  let cmd = head cliArgs
  let args = tail cliArgs
  runCmd cmd args
