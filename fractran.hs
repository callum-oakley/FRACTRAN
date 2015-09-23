-- see https://en.wikipedia.org/wiki/FRACTRAN

-- to do: multipe parameters

import Data.Maybe
import Data.Ratio
import Data.Word

type Game = [Ratio Word]

divides :: Word -> Word -> Bool
a `divides` b = b `mod` a == 0

isPowerOf :: Word -> Word -> Bool
isPowerOf b n = minimax == n
  where minimax = head . filter (>=n) . map (b^) $ [0..]

wordLog :: Word -> Word -> Word
wordLog b n = head . filter (\i -> b^i >= n) $ [0..]

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

fractran :: Game -> Maybe Word -> Maybe Word
fractran _ Nothing = Nothing
fractran game m =
    fmap (\q -> numerator (q * ((fromJust m) % 1))) . maybeHead .
    filter (\q -> denominator q `divides` fromJust m) $ game

run :: Word -> Word -> Game -> Word -> [Word]
run startBase endBase game initial =
    filterPowersAndTakeLogs . takeWhileJust $ computation
  where filterPowersAndTakeLogs =
            map (wordLog endBase) . filter (isPowerOf endBase)
        takeWhileJust = map fromJust . takeWhile isJust
        computation = iterate (fractran game) (Just (startBase^initial))

primeGame :: Game -- 2 2
primeGame = [
    17%91,
    78%85,
    19%51,
    23%38,
    29%33,
    77%29,
    95%23,
    77%19,
    1 %17,
    11%13,
    13%11,
    15% 2,
    1 % 7,
    55% 1]

hammingGame :: Game -- 2 13
hammingGame = [
    33%20,
    5 %11,
    13%10,
    1 % 5,
    2 % 3,
    10% 7,
    7 % 2]

kilminsterGame :: Game -- 10 10
kilminsterGame = [
    7 %  3,
    99% 98,
    13% 49,
    39% 35,
    36% 91,
    10%143,
    49% 13,
    7 % 11,
    1 %  2,
    91%  1]

kilminsterGame' :: Game -- 10 10
kilminsterGame' = [
    3  % 11,
    847% 45,
    143%  6,
    7  %  3,
    10 % 91,
    3  %  7,
    36 %325,
    1  %  2,
    36 %  5]
