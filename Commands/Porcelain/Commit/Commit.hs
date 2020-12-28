module Commands.Porcelain.Commit.Commit where

import Commands.Plumbing.Commit.CommitFromTree

import Core.Core
import Core.Index
import Core.Object.Object
import Core.Object.Tree
import Core.Reference

import qualified Data.ByteString.Char8 as B

import Data.Either
import Data.Time
import System.Directory
import System.Process

cmdCommit :: Command
cmdCommit _ = do
  let msgPath = ".git/COMMIT_EDITMSG"

  -- Get commit metadata
  editor <- defaultEditor
  author <- authorName
  email  <- authorEmail
  now    <- getZonedTime

  -- Edit commit message
  callProcess editor [msgPath]
  msg <- readFile msgPath
  removeFile msgPath
  if msg == "" then fail "" else return ()

  -- Generate and store tree and subtrees
  index <- readIndex
  let objs = treesFromIndex index
  let tree = hashObject . head $ objs
  let objsIOs = map storeObject objs
  sequence_ $ objsIOs

  -- Branch and parent
  head <- getHead
  if isLeft head then return () else fail "HEAD detached"
  let branch = fromLeft "" head
  parent <- getBranchCommitHash branch

  -- Store commit
  let commit = Commit tree [parent] author email now msg
  let hash = hashObject commit
  sequence_ [storeObject commit, putStrLn $ B.unpack hash]

  -- Update branch
  updateBranch branch hash
