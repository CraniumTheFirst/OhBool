{-# LANGUAGE MultiWayIf #-}
module OhBool.Parser where

import OhBool.Common

newtype ValidString = ValidString String
                    deriving Show

parse :: String -> Maybe Expression
parse s = parse' s Empty

parse' :: String -> Expression -> Maybe Expression
parse' _ _ = undefined

validateString :: String -> Bool
validateString s = checkParenthesis s &&
                   checkParenthesis s

checkParenthesis :: String -> Bool
checkParenthesis = check' (0 :: Int)
  where check' n "" = n == 0
        check' n (x:xs) = if | n < 0 -> False
                             | x == '(' -> check' (n+1) xs
                             | x == ')' -> check' (n-1) xs
                             | otherwise -> check' n xs
