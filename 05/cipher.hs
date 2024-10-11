-- Exercise 10
import Data.Char

affine_mod :: Int -> Int -> Int -> Int
affine_mod min max value = ((value - min) `mod` (max - min + 1)) + min

shift_int :: Int -> Int -> Int -> Int -> Int
shift_int min max shift_value n = affine_mod min max (shift_value + n)

shift :: Int -> Char -> Char
shift shift_value c
    | isLower c = chr (shift_int (ord 'a') (ord 'z') shift_value (ord c))
    | isUpper c = chr (shift_int (ord 'A') (ord 'Z') shift_value (ord c))
    | otherwise = c

encode :: Int -> String -> String
encode n xs = [shift n x | x <- xs]
