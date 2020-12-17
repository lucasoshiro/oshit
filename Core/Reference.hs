module Core.Reference where

import Core.Core
import Util.Util

import qualified Data.ByteString.Char8 as B
import System.Directory

type Branch = String

branchDir :: FilePath
branchDir = ".git/refs/heads/"

branchPath :: Branch -> FilePath
branchPath branch = branchDir ++ branch

headPath :: FilePath
headPath = ".git/HEAD"

updateHead :: Either Branch Hash -> IO ()
updateHead (Left branch) = writeFile headPath $
  "ref: " ++ "refs/heads/" ++ branch ++ "\n"
updateHead (Right hash) = writeFile headPath $ B.unpack hash ++ "\n"


updateBranch :: Branch -> Hash -> IO ()
updateBranch branch hash = writeFile (branchPath branch) $ B.unpack hash

setBranchToHead :: Branch -> IO ()
setBranchToHead branch = writeFile ".git/HEAD" $ "ref: " ++
                         "refs/heads/" ++ (branchPath branch)

setCommitToHead :: Hash -> IO ()
setCommitToHead hash = writeFile ".git/HEAD" $ B.unpack hash

allBranches :: IO [(Branch, Hash)]
allBranches = do
  branches <- listDirectory branchDir
  sequence [ do
               hash <- readFile $ branchPath branch
               return (branch, B.pack . trim $ hash)
           | branch <- branches
           ]
