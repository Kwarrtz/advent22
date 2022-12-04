import Data.Char
import Data.List

priority :: Char -> Int
priority c
    | isAsciiUpper c = ord c - 38
    | isAsciiLower c = ord c - 96
    | otherwise = 0

run1 :: String -> Int
run1 = sum . map common . lines
    where
        common :: String -> Int
        common s = priority $ head $ uncurry intersect $ splitAt (length s `div` 2) s

intersection :: Eq a => [[a]] -> [a]
intersection [] = []
intersection [xs] = xs
intersection [xs,ys] = intersect xs ys
intersection (xs:xss) = intersect xs $ intersection xss

splitBy :: Int -> [a] -> [[a]]
splitBy n xs
    | length xs <= n = [xs]
    | otherwise = case splitAt n xs of (fst, rst) -> fst : splitBy n rst        
        
run2 :: String -> Int
run2 = sum . map (priority . head . intersection) . splitBy 3 . lines

main :: IO ()
main = do
    input <- readFile "input3.txt"
    putStrLn . show $ run1 input
    putStrLn . show $ run2 input