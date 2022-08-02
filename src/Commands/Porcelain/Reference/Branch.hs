module Commands.Porcelain.Reference.Branch where

import Core.Core
import Core.Reference
import Util.Colors

import qualified Data.Set              as Set

import Data.Either

cmdBranch :: Command

-- No branch provided, show all branches:
cmdBranch [] = do
  head' <- getHead
  let currentBranch = fromLeft "" head'
  branches <- allBranches >>= return . map fst

  sequence_ $ do
    branch <- branches
    if currentBranch == branch
      then [putStrLn $ "* " ++ colorize green branch]
      else [putStrLn $ "  " ++ branch]

-- Branch provided, create branch
cmdBranch (branch:_) = do
  branches <- allBranches >>= return . map fst
  let branchSet = Set.fromList branches

  if branch `Set.member` branchSet
    then fail $ "Branch " ++ branch ++ " exists."
    else return ()

  headCommit <- getHeadCommitHash
  updateBranch branch headCommit

