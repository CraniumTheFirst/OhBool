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
hammingDistance = hammingDistance' 0

hammingDistance' :: (Eq a) => Int -> [a] -> [a] -> Int
hammingDistance' acc [] ys = acc + length ys
hammingDistance' acc xs [] = acc + length xs
hammingDistance' acc (x:xs) (y:ys) = if x == y 
                                         then hammingDistance' acc xs ys 
                                         else hammingDistance' (acc+1) xs ys 

numHammingDistance :: Bits a => a -> a -> Int
numHammingDistance x y = popCount (x `xor` y)

pairs :: Int -> [a] -> [(a,a)]
pairs n xs = zip xs (drop n xs)
