module Core.Reference where

import Core.Core

import qualified Data.ByteString.Char8 as B

type Branch = String

branchPath :: Branch -> FilePath
branchPath branch = ".git/refs/heads/" ++ branch

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
