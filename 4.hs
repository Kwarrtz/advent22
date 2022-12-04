import Text.ParserCombinators.Parsec
import Control.Monad
import Data.Bool
import Data.Either

data Range = Range Int Int

inputFile :: GenParser Char st [(Range,Range)]
inputFile = pair `endBy` eol
    where
        pair = do
            as1 <- assignment
            char ','
            as2 <- assignment
            return (as1,as2)
        assignment = do
            a <- num
            char '-'
            b <- num
            return $ Range a b
        num = read <$> many1 digit
        eol = void (char '\n') <|> eof

contains :: Range -> Range -> Bool
contains (Range a1 b1) (Range a2 b2) = 
    (a1 <= a2) && (b1 >= b2)

redundant :: Range -> Range -> Bool
redundant x y = x `contains` y || y `contains` x

run1 :: [(Range,Range)] -> Int
run1 = sum . map (bool 0 1 . uncurry redundant)

overlap :: Range -> Range -> Bool
overlap (Range a1 b1) (Range a2 b2) =
    (a1 <= b2) && (a2 <= b1)

run2 :: [(Range,Range)] -> Int
run2 = sum . map (bool 0 1 . uncurry overlap)

main :: IO ()
main = do
    input <- readFile "input4.txt"
    let parsed = fromRight [] $ parse inputFile "" input
    putStrLn . show $ run1 parsed
    putStrLn . show $ run2 parsed