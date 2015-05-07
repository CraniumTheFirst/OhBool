module OhBool.Utils where

import Data.Bits (Bits, shiftR, xor, popCount)
import Data.List (sortBy)
import Data.Function (on)

import Codec.Binary.Gray.Bits (binary)

safeHead :: [a] -> Maybe a
safeHead [] = Nothing
safeHead (x:_) = Just x

grayify :: (Ord a, Eq a) => [a] -> [a]
grayify xs = map snd $ sortBy (compare `on` fst) $ zipWith (\n e -> (binary n,e)) ([0..] :: [Int]) xs

hammingDistance :: (Eq a) => [a] -> [a] -> Int
hammingDistance xs ys = length $ filter id $ zipWith (/=) xs ys

numHammingDistance :: Bits a => a -> a -> Int
numHammingDistance x y = popCount (x `xor` y)

pairs :: Int -> [a] -> [(a,a)]
pairs n xs = zip xs (drop n xs)
