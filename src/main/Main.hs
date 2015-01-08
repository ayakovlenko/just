module Main where

import System.Environment (getArgs)
import System.IO
import Data.List (intercalate)
import Utils

format :: Int -> String -> String
format n text = intercalate "\n" $ map (buildParagraph n . breakToLines n) $ paragraphs text

main :: IO ()
main = do
  [n] <- getArgs
  hSetBuffering stdin LineBuffering
  interact $ format (read n :: Int)
