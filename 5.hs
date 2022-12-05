import Text.ParserCombinators.Parsec
import Data.Functor
import Control.Monad
import Data.List
import Data.Maybe
import Data.Either

data Move = Move Int Int Int
    deriving Show

type Stacks = [[Char]]

inputFile :: GenParser Char st (Stacks, [Move])
inputFile = liftM2 (,) stacks moves <* eof
    where
        stacks = do
            blocksT <- (try maybeBlock `sepBy` char ' ') `endBy` eol
            string " 1   2   3   4   5   6   7   8   9 "; eol
            eol
            return $ map catMaybes $ transpose $ blocksT
        maybeBlock = (Just <$> block) <|> (string "   " $> Nothing)
        block = char '[' *> upper <* char ']'
        moves = move `endBy` eol
        move = do
            string "move "
            n <- number
            string " from "
            from <- number
            string " to "
            to <- number
            return $ Move n from to
        number = read <$> many digit
        eol = char '\n'

mapFst :: (a -> b) -> (a, c) -> (b, c)
mapFst f (a, b) = (f a, b)

mapSnd :: (a -> b) -> (c, a) -> (c, b)
mapSnd f (a, b) = (a, f b)

lift :: Int -> Int -> Stacks -> ([Char], Stacks)
lift n 1 (s : ss) = mapSnd (: ss) $ splitAt n s
lift n i (s : ss) = mapSnd (s :) $ lift n (i-1) ss

place :: Int -> [Char] -> Stacks -> Stacks
place 1 cs (s : ss) = (cs ++ s) : ss
place n cs (s : ss) = s : place (n-1) cs ss

moveSerial :: Stacks -> Move -> Stacks
moveSerial ss (Move n from to) = uncurry (place to) $ mapFst reverse $ lift n from ss

moveSimul :: Stacks -> Move -> Stacks
moveSimul ss (Move n from to) = uncurry (place to) $ lift n from ss

main = do
    rawInput <- readFile "input5.txt"
    let input = fromRight (error "parse error") $ parse inputFile "" rawInput
    sequence $ [moveSerial, moveSimul] <&> (\move -> 
        putStrLn $ map head $ uncurry (foldl' move) input) 