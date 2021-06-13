module Main where

import Data.Csv (decodeByName)
import qualified Data.ByteString.Lazy as BL (readFile, writeFile)
import Data.Foldable (toList)
import QuoteData

main :: IO ()
main = putStrLn "foo"

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
  csvData <- BL.readFile fpath
  case decodeByName csvData of
    Left err -> error err
    Right (_, quotes) -> pure (toList quotes)
