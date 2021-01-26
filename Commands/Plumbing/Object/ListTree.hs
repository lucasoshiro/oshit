module Commands.Plumbing.Object.ListTree where

import Core.Core
import Core.Object.Object
import Core.Object.Tree

import qualified Data.ByteString.Char8 as B

cmdListTree :: Command
cmdListTree [hash'] = do
  let hash = B.pack hash'
  tree <- loadObject hash :: IO Tree
  contents <- listTreeRecursive tree
  sequence_ . map putStrLn $ contents
cmdListTree _ = fail "tree not provided"
