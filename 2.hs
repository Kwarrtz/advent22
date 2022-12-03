import Data.List
import Data.Maybe

data Move = Rock | Paper | Scissors
    deriving Eq 

data Round = Round Move Move

instance Ord Move where
    (<=) Rock Scissors = False
    (<=) Rock _ = True
    (<=) Paper Rock = False
    (<=) Paper _ = True
    (<=) Scissors Paper = False
    (<=) Scissors _ = True

score :: Round -> Integer
score (Round op me) = moveVal + outcomeVal
    where
        moveVal = case me of
            Rock -> 1
            Paper -> 2
            Scissors -> 3
        outcomeVal = case compare me op of
            LT -> 0
            EQ -> 3
            GT -> 6

parseRound :: String -> Maybe Round
parseRound [op,' ',me] = do
    op' <- case op of
        'A' -> Just Rock
        'B' -> Just Paper
        'C' -> Just Scissors
        _ -> Nothing
    me' <- case me of
        'X' -> Just Rock
        'Y' -> Just Paper
        'Z' -> Just Scissors
        _ -> Nothing
    return $ Round op' me'
parseRound _ = Nothing

run :: (String -> Maybe Round) -> String -> Integer
run parser = sum . map (fromMaybe 0 . fmap score . parser) . lines

forResult :: Ordering -> Move -> Move
forResult ord op = fromJust $ find (\me -> compare me op == ord) [Rock, Paper, Scissors]

parseRound2 :: String -> Maybe Round
parseRound2 [op, ' ', res] = do
    op' <- case op of
        'A' -> Just Rock
        'B' -> Just Paper
        'C' -> Just Scissors
        _ -> Nothing
    res' <- case res of
        'X' -> Just LT
        'Y' -> Just EQ
        'Z' -> Just GT
        _ -> Nothing
    return $ Round op' (forResult res' op')
parseRound2 _ = Nothing

main :: IO ()
main = do
    input <- readFile "input2.txt"
    putStrLn . show $ run parseRound input
    putStrLn . show $ run parseRound2 input