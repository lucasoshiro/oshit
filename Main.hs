import Core.Core

import Commands.Plumbing.Object.CreateBlob
import Commands.Plumbing.Object.ShowObj
import Commands.Porcelain.Init


import System.Environment


runCmd :: String -> Command
runCmd "init" = cmdInit
runCmd "create-blob" = createBlob
runCmd "show-obj" = showObj
runCmd _ = \_ -> fail "invalid command"

main :: IO ()
main = do
  cliArgs <- getArgs
  let cmd = head cliArgs
  let args = tail cliArgs
  runCmd cmd args
