module Commands.Plumbing.Packfile.VerifyPack where

import Core.Core
import Core.Packfile

import Data.List

import qualified Data.ByteString.Char8      as B

cmdVerifyPack :: Command
cmdVerifyPack (hash':_) = do
  let hash = B.pack hash'
  verif <- verifyPack hash

  putStrLn . intercalate "\n" $ [unwords [B.unpack h, show t, show s] | (h, t, s) <- verif]
cmdVerifyPack _ = fail "packfile hash not provided"
