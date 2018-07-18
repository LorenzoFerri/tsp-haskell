module Parse where

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Combinator
import Text.ParserCombinators.Parsec.Char
import Text.ParserCombinators.Parsec.Number
import Data.Matrix
import Definitions

parseInput :: Parser DM
parseInput = do
    manyTill anyChar (try (string "NODE_COORD_SECTION\n"))
    cityList <- many parseCity
    return $ calculateDM cityList

parseCity :: Parser City
parseCity = do
    spaces
    int
    spaces
    x <- parsePosition
    spaces
    y <- parsePosition
    spaces
    return (x,y)

parsePosition :: Parser Int
parsePosition = try parseFloat Text.Parsec.<|>
                try parseInt <?>
                "Error parsing the position"

parseInt :: Parser Int
parseInt = int

parseFloat :: Parser Int
parseFloat = round <$> floating

calculateDM :: [City] -> DM
-- calculateDM cityList = map (\x -> map (\y -> euclidianDistance x y) cityList) cityList
calculateDM cityList = do
    let n = length cityList
    matrix n n $ \(i,j) -> euclidianDistance (cityList!!(i-1)) (cityList!!(j-1))

euclidianDistance :: City -> City -> Int
euclidianDistance (x1 , y1) (x2 , y2) = round $ sqrt $ fromIntegral (x'*x' + y'*y')
    where
        x' = x1 - x2
        y' = y1 - y2
    