import Data.Ratio
import Data.List
import Data.Maybe

fractran :: [Rational] -> Integer -> [Integer]
fractran program input = case find (\f -> numerator f * input `mod` denominator f == 0) program of 
    Just frac -> let res =  numerator frac * input `div` denominator frac in res : fractran program res
    Nothing   -> []

main = print (fractran  [(455%33), (11%13), (1%11), (3%7), (11%2), (1%3)] 60466176)