module Chatbot where

import System.Random

  -- Everything after my
  -- "my" "me" "I"

-- Words to questions.
qtoA [] = []
qtoA (x:xs)
  | x == "I" = "you" : qtoA xs
  | x == "am" = "are" : qtoA xs
  | x == "my" = "your" : qtoA xs
  | x == "me" = "you" : qtoA xs
  | x == "US" = "Korea" : qtoA xs
  | x == "American" = "Jamaican" : qtoA xs
  | x == "Jiin" = "Jeffrey" : qtoA xs
  | x == "Jeffrey" = "Jiin" : qtoA xs
  | x == "Jeong" = "Friedman" : qtoA xs
  | x == "Clarke" = "Helmuth" : qtoA xs
  | x == "Tom" = "Tim" : qtoA xs
  | otherwise = x : qtoA xs

-- Randomly generates questions.
question sentence num
  | num `mod` 4 == 1 = "Jiinfrey Chat"
  | num `mod` 4 == 2 = "Why do you say " ++ unwords(qtoA $ words sentence) ++ "?"
  | num `mod` 4 == 3 = "Tell me more about"
  | otherwise = "Hi I am random."

-- Replies user with a question.
ask gen = do
  sentence <- getLine
  if null sentence
    then return ()
    else do
      -- Or we can use randomR.
      putStrLn $ question sentence $ fst $ (random gen :: (Int, StdGen))
      ask $ snd $ (random gen :: (Int, StdGen))

-- Starts Jiinffrey chat.
main = do
  putStrLn "Welcome! I am Jiinffrey Chat!"
  gen <- getStdGen
  ask gen
