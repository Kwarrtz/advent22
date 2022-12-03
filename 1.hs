import Data.List

splitOnEmpty :: [String] -> [[String]]
splitOnEmpty = snd . foldr go (False,[])
    where
        go :: String -> (Bool, [[String]]) -> (Bool, [[String]])
        go "" (_, buf) = (False, buf)
        go s (True, (cur : buf)) = (True, (s : cur) : buf)
        go s (False, buf) = (True, [s] : buf)

run :: String -> (Integer, Integer)
run s = (head elves, sum . take 3 $ elves)
    where elves = reverse . sort . map (sum . map read) . splitOnEmpty . lines $ s

main :: IO ()
main = (show . run) <$> readFile "input1.txt" >>= putStrLn
