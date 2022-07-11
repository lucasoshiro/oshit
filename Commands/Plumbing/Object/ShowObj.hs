module Commands.Plumbing.Object.ShowObj where

import Core.Core
import Core.Object

import qualified Data.ByteString.Char8 as B

cmdShowObj :: Command
cmdShowObj (hash:_) = do
  rawObj <- loadRawObject $ B.pack hash
  let objType = rawObjectType $ rawObj

  case objType of Just BlobType -> do
                    obj <- objectParse rawObj :: IO Blob
                    putStrLn . objectPretty $ obj

                  Just CommitType-> do
                    obj <- objectParse rawObj :: IO Commit
                    putStrLn . objectPretty $ obj

                  Just TreeType -> do
                    obj <- objectParse rawObj :: IO Tree
                    putStrLn . objectPretty $ obj

                  Nothing -> fail "invalid object"

  
  -- B.putStr $ objectRawContent obj
cmdShowObj _ = fail "hash missing"
