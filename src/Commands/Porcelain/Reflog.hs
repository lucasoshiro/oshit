module Commands.Porcelain.Reflog where

import Core.Core
import Core.Reflog
import Core.Reference
import Util.Colors

import Data.List

import qualified Data.ByteString.Char8 as B

cmdReflog :: Command
cmdReflog (ref:_) = do
  isBranch <- branchExists ref
  let isStash = ref == "stash"

  reflog <- if isBranch
            then readBranchReflog ref
            else if isStash
                 then readStashReflog
                 else readSymrefReflog ref

  putStrLn . prettyReflog reflog $ ref
cmdReflog [] = cmdReflog ["HEAD"]

prettyReflog :: Reflog -> String -> String
prettyReflog reflog reference = intercalate "\n" prettyEntries
  where prettyEntries = zipWith prettyReflogEntry [0..] . reverse $ reflog 

        prettyReflogEntry :: Int -> ReflogEntry -> String
        prettyReflogEntry index entry = unwords [colorize yellow shortHash, relative, title entry]
          where shortHash = B.unpack . B.take 7 $ newHash entry
                relative = reference ++ "@{" ++ show index ++ "}:"
