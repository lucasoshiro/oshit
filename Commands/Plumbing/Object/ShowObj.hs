module Commands.Plumbing.Object.ShowObj where

import Core.Core
import Core.Object

import qualified Data.ByteString.Char8 as B

cmdShowObj :: Command
cmdShowObj (hash:_) = do
  rawObj <- loadRawObject $ B.pack hash
  let objType = B.unpack . rawObjectTypeLegacy $ rawObj

  case objType of "blob" -> do
                    obj <- objectParse rawObj :: IO Blob
                    putStrLn . objectPretty $ obj

                  "commit" -> do
                    obj <- objectParse rawObj :: IO Commit
                    putStrLn . objectPretty $ obj

                  "tree" -> do
                    obj <- objectParse rawObj :: IO Tree
                    putStrLn . objectPretty $ obj

                  _ -> fail "invalid object"

  
  -- B.putStr $ objectRawContent obj
cmdShowObj _ = fail "hash missing"
