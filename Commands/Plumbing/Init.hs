module Commands.Plumbing.Init where

import Core.Core
import System.Directory

cmdInit :: Command
cmdInit _ = do
  createDirectory ".git"
  createDirectory ".git/.objects"

