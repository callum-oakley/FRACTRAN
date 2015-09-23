-- see https://en.wikipedia.org/wiki/FRACTRAN

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

run :: Game -> Word -> [Word]
run game initial = filterPowersAndTakeLogs . takeWhileJust $ computation
  where filterPowersAndTakeLogs = map (wordLog 2) . filter (isPowerOf 2)
        takeWhileJust = map fromJust . takeWhile isJust
        computation = iterate (fractran game) (Just (2^initial))

primeGame :: Game
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

main = mapM_ (putStrLn . show) (run primeGame 1)
