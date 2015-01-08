module Utils where

import Data.List (concatMap, intercalate)
import Data.List.Split (splitOn)

decode :: [Int] -> [String]
decode = map (`replicate` ' ')

distribute :: Int -> Int -> [Int]
distribute _ 0 = []
distribute n over = x : distribute (n - x) (over - 1)
  where x = n `div` over

wordsLength :: [String] -> Int
wordsLength = sum . map length

intersperseJoin :: [String] -> [String] -> String
intersperseJoin (x:_) []     = x
intersperseJoin (x:xs) (y:ys) = x ++ y ++ intersperseJoin xs ys

buildLine :: Int -> [String] -> String
buildLine just wordz =
  let sepMin = length wordz - 1
      sepMax = just - wordsLength wordz
      separators = decode $ distribute sepMax sepMin
  in intersperseJoin wordz separators

linesToWords :: [String] -> [String]
linesToWords = concatMap words

breakToLines :: Int -> [String] -> [[String]]
breakToLines just wordz = breakToLines' wordz [] []
  where
    breakToLines' :: [String] -> [String] -> [[String]] -> [[String]]
    breakToLines' [] lineAcc parAcc = parAcc ++ [lineAcc]
    breakToLines' wordz'@(w:ws) lineAcc parAcc =
      let
        sepMin = length lineAcc - 1
        charsLeft = just - sepMin - wordsLength lineAcc
      in if length w < charsLeft
        then breakToLines' ws (lineAcc ++ [w]) parAcc
        else breakToLines' wordz' [] (parAcc ++ [lineAcc])

buildParagraph :: Int -> [[String]] -> String
buildParagraph _ [] = "\n"
buildParagraph _ [ln] = unwords ln ++ "\n"
buildParagraph just (ln:lns) = buildLine just ln ++ "\n" ++ buildParagraph just lns

paragraphs :: String -> [[String]]
paragraphs s = filter (not . null) $ map words $ splitOn "\n\n" s
