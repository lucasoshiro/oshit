module Commands.Porcelain.Init where

import Core.Core
import Core.Reference

import System.Directory

cmdInit :: Command
cmdInit _ = do
  createDirectory ".git"
  createDirectory ".git/branches"
  createDirectory ".git/hooks"
  createDirectory ".git/info"
  createDirectory ".git/objects"
  createDirectory ".git/objects/info"
  createDirectory ".git/objects/pack"
  createDirectory ".git/refs"
  createDirectory ".git/refs/heads"
  createDirectory ".git/refs/tags"

  updateHead $ Left "default"

  writeFile ".git/config" $
    "[core]\n\
    \        repositoryformatversion = 0\n\
    \        filemode = true\n\
    \        bare = false\n\
    \        logallrefupdates = true\n"

  writeFile ".git/description" ""
  writeFile ".git/info/exclude" ""
