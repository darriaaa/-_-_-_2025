module Main where

import Data.Char (isAlpha, toLower)
import Data.List (sortOn)

--------------------------------------------------
-- Власні типи
--------------------------------------------------

type Symbol = Char
type WordT  = String

data Punctuation
  = Dot
  | Excl
  | Quest
  deriving (Show, Eq)

newtype Sentence = Sentence [WordT]
  deriving (Show, Eq)

--------------------------------------------------
-- Допоміжні функції
--------------------------------------------------

-- Перевірка, чи є символ кінцем речення
isSentencePunct :: Symbol -> Bool
isSentencePunct c = c `elem` (".!?" :: String)

-- Обрізання пробілів
trim :: String -> String
trim = dropWhileEnd . dropWhileStart
  where
    dropWhileStart = dropWhile (`elem` (" \t\n\r" :: String))
    dropWhileEnd   = reverse . dropWhile (`elem` (" \t\n\r" :: String)) . reverse

-- Розбиття тексту на "сирі" речення
splitSentencesRaw :: String -> [String]
splitSentencesRaw = go "" []
  where
    go acc res [] =
      let acc' = trim acc
      in if null acc' then res else res ++ [acc']

    go acc res (c:cs)
      | isSentencePunct c =
          let acc' = trim acc
          in if null acc'
             then go "" res cs
             else go "" (res ++ [acc']) cs
      | otherwise =
          go (acc ++ [c]) res cs

-- Розбиття речення на слова
sentenceToWords :: String -> [WordT]
sentenceToWords s =
  let lower  = map toLower s
      groups = wordsBy (not . isAlpha) lower
  in filter (not . null) groups

-- Аналог splitOn за предикатом
wordsBy :: (Char -> Bool) -> String -> [String]
wordsBy p str = case dropWhile p str of
  "" -> []
  s' ->
    let (w, rest) = break p s'
    in w : wordsBy p rest

-- Перетворення тексту в список структурованих речень
parseSentences :: String -> [Sentence]
parseSentences txt =
  [ Sentence (sentenceToWords raw)
  | raw <- splitSentencesRaw txt
  ]

--------------------------------------------------
-- Основне завдання
--------------------------------------------------

-- Кількість слів у реченні
sentenceLength :: Sentence -> Int
sentenceLength (Sentence ws) = length ws

-- Сортування речень за кількістю слів
sortSentencesByLength :: [Sentence] -> [Sentence]
sortSentencesByLength = sortOn sentenceLength

--------------------------------------------------
-- main
--------------------------------------------------

main :: IO ()
main = do
  putStrLn "Reading text from file input.txt..."
  text <- readFile "input.txt"

  let sentences = parseSentences text
      sorted    = sortSentencesByLength sentences

  putStrLn "\nSentences sorted by number of words (ascending):\n"

  mapM_ printSentence sorted

--------------------------------------------------
-- Вивід речення
--------------------------------------------------

printSentence :: Sentence -> IO ()
printSentence (Sentence ws) = do
  putStrLn $ show (length ws) ++ " words: " ++ unwords ws
