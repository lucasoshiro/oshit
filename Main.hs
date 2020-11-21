import Core.Core

import Commands.Plumbing.Init

import System.Environment


runCmd :: String -> Command
runCmd "init" = cmdInit
runCmd _ = \_ -> fail "invalid command"

main :: IO ()
main = do
  cliArgs <- getArgs
  let cmd = head cliArgs
  let args = tail cliArgs
  runCmd cmd args
