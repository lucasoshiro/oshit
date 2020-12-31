import Core.Core

import Commands.Plumbing.Commit.CommitFromTree
import Commands.Plumbing.Index.AddBlob
import Commands.Plumbing.Index.ListIndex
import Commands.Plumbing.Index.TreeFromIndex
import Commands.Plumbing.Object.CreateBlob
import Commands.Plumbing.Object.ShowObj
import Commands.Plumbing.Reference.UpdateBranch
import Commands.Plumbing.Reference.UpdateHead

import Commands.Porcelain.Commit.Commit
import Commands.Porcelain.Init
import Commands.Porcelain.Log
import Commands.Porcelain.Reference.Branch

import System.Environment


runCmd :: String -> Command
runCmd "add-blob"         = cmdAddBlob
runCmd "branch"           = cmdBranch
runCmd "commit-from-tree" = cmdCommitFromTree
runCmd "create-blob"      = cmdCreateBlob
runCmd "commit"           = cmdCommit
runCmd "init"             = cmdInit
runCmd "log"              = cmdLog
runCmd "list-index"       = cmdListIndex
runCmd "show-obj"         = cmdShowObj
runCmd "tree-from-index"  = cmdTreeFromIndex
runCmd "update-branch"    = cmdUpdateBranch
runCmd "update-head"      = cmdUpdateHead
runCmd _ = \_ -> fail "invalid command"

main :: IO ()
main = do
  cliArgs <- getArgs
  let cmd = head cliArgs
  let args = tail cliArgs
  runCmd cmd args
