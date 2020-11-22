module Core.Core where

import qualified Data.ByteString.Char8 as B

type Command = [String] -> IO()
type Hash = B.ByteString
