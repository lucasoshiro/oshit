module Commands.Porcelain.Reference.Checkout where

import Core.Core

cmdCheckout :: Command
cmdCheckout (ref:_) = do
  
