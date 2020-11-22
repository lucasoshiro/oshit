module Core.Stage where

import qualified Data.ByteString.Char8 as B
import System.Directory

import Core.Core

type Stage = [(Hash, FilePath)]

stagePath :: FilePath
stagePath = ".git/oshit_stage"

unparseStage :: Stage -> B.ByteString
unparseStage stage = B.intercalate (B.pack "\n") $
  [ B.concat[hash, B.pack " ", B.pack path]
  | (hash, path) <- stage
  ]

parseStage :: B.ByteString -> Stage
parseStage bs = [ (line !! 0, B.unpack $ line !! 1)
                | line <- map (B.split ' ') $ B.split '\n' bs
                ]

storeStage :: Stage -> IO ()
storeStage = B.writeFile stagePath . unparseStage

loadStage :: IO Stage
loadStage = do
  exists <- doesFileExist stagePath 
  if exists
    then B.readFile stagePath >>= return . parseStage
    else return []

appendBlob :: Stage -> Hash -> FilePath -> Stage
appendBlob stage hash filepath = (hash, filepath):stage
