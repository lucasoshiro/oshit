module Commands.Porcelain.Index.Status where

import Core.Core
import Core.Index
import Util.Colors

import Control.Monad

cmdStatus :: Command
cmdStatus _ = do
  (modified, deleted, untracked) <- indexStatus

  if modified /= []
    then do
    putStrLn "Modified: "
    sequence_ [putStrLn . colorize green $ file | file <- modified]
    else return ()

  if deleted /= []
    then do
    putStrLn "Deleted: "
    sequence_ [putStrLn . colorize green $ file | file <- deleted]
    else return ()

  if untracked /= []
    then do
    putStrLn "Untracked: "
    sequence_ [putStrLn . colorize green $ file | file <- untracked]
    else return ()
