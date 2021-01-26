module Core.Reference where

import Core.Core
import Util.Util

import qualified Data.ByteString.Char8 as B
import qualified System.Directory      as Dir

import Data.List
import Data.List.Split

type Branch = String

branchDir :: FilePath
branchDir = ".git/refs/heads/"

branchPath :: Branch -> FilePath
branchPath branch = branchDir ++ branch

branchExists :: Branch -> IO Bool
branchExists = Dir.doesFileExist . branchPath

headPath :: FilePath
headPath = ".git/HEAD"

getHead :: IO (Either Branch Hash)
getHead = do
  content <- readFile headPath >>= return . trim
  return $
    if "ref" `isPrefixOf` content
    then Left . last $ (splitOn "/" content)
    else Right . B.pack $ content
  
getHeadCommitHash :: IO Hash
getHeadCommitHash = do
  content <- getHead
  case content of
    Right hash -> return hash
    Left branch -> getBranchCommitHash branch

getBranchCommitHash :: Branch -> IO Hash
getBranchCommitHash branch = (readFile . branchPath $ branch) >>=
                             return . B.pack . trim

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
  branches <- Dir.listDirectory branchDir
  sequence [ do
               hash <- readFile $ branchPath branch
               return (branch, B.pack . trim $ hash)
           | branch <- branches
           ]
