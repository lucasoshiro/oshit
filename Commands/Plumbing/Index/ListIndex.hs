module Commands.Plumbing.Index.ListIndex where

import Core.Core
import Core.Index

cmdListIndex :: Command
cmdListIndex _ = readIndex >>= showIndex
