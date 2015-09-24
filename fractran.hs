-- see https://en.wikipedia.org/wiki/FRACTRAN

import Data.Maybe
import Data.Ratio

type Instructions = [Rational]

data Game = Game {
    startBases    :: [Integer],
    endBase       :: Integer,
    instructions  :: Instructions
}

isPowerOf :: Integer -> Integer -> Bool
isPowerOf b n = minimax == n
  where
    minimax = head . filter (>=n) . map (b^) $ [0..]

integerLog :: Integer -> Integer -> Integer
integerLog b n = head . filter (\i -> b^i >= n) $ [0..]

maybeHead :: [a] -> Maybe a
maybeHead [] = Nothing
maybeHead (x:_) = Just x

fractran :: Instructions -> Maybe Integer -> Maybe Integer
fractran _ Nothing = Nothing
fractran instructions m =
    fmap (\q -> numerator (q * ((fromJust m) % 1))) . maybeHead .
    filter (\q -> mod (fromJust m) (denominator q) == 0) $ instructions

run :: Game -> [Integer] -> [Integer]
run game parameters = filterAndTakeLogs . takeWhileJust $ computation
  where
    filterAndTakeLogs = map (integerLog endBase') . filter (isPowerOf endBase')
    takeWhileJust = map fromJust . takeWhile isJust
    computation = iterate (fractran instructions') initial
    startBases' = startBases game
    endBase' = endBase game
    instructions' = instructions game
    initial = Just . product . zipWith (^) startBases' $ parameters

-- takes input [a, b] and outputs [a + b]
additionGame :: Game
additionGame = Game {
    startBases = [2, 3],
    endBase = 3,
    instructions = [
        3 % 2
    ]
}

-- takes input [a, b] and outputs [a * b]
multiplicationGame :: Game
multiplicationGame = Game {
    startBases = [2, 3],
    endBase = 5,
    instructions = [
        455 % 33,
        11  % 13,
        1   % 11,
        3   %  7,
        11  %  2,
        1   %  3
    ]
}

-- takes input [a, b] and outputs [a - b]
subtractionGame :: Game
subtractionGame = Game {
    startBases = [2, 3],
    endBase = 2,
    instructions = [
        1 % 6
    ]
}

-- takes input [a, b] and outputs [a / b]
divisionGame :: Game
divisionGame = Game {
    startBases = [2, 3],
    endBase = 5,
    instructions = [
        13  % 17,
        119 % 78,
        1   % 39,
        5   % 13,
        26  % 11,
        3   %  7,
        11  %  2,
        1   %  3
    ]
}

-- given input [p] for p prime outputs a list of primes >= p in order
primeGame :: Game
primeGame = Game {
    startBases = [10],
    endBase = 10,
    instructions = [
        3   %  11,
        847 %  45,
        143 %   6,
        7   %   3,
        10  %  91,
        3   %   7,
        36  % 325,
        1   %   2,
        36  %   5
    ]
}

-- given input [a] outputs the hamming weight of a, [h(a)]
hammingGame :: Game
hammingGame = Game {
    startBases = [2],
    endBase = 13,
    instructions = [
        33 % 20,
        5  % 11,
        13 % 10,
        1  %  5,
        2  %  3,
        10 %  7,
        7  %  2
    ]
}

-- another prime generator
conwayGame :: Game
conwayGame = Game {
    startBases = [2],
    endBase = 2,
    instructions = [
        17 % 91,
        78 % 85,
        19 % 51,
        23 % 38,
        29 % 33,
        77 % 29,
        95 % 23,
        77 % 19,
        1  % 17,
        11 % 13,
        13 % 11,
        15 %  2,
        1  %  7,
        55 %  1
    ]
}

-- yet another prime generator
kilminsterGame :: Game
kilminsterGame = Game {
    startBases = [10],
    endBase = 10,
    instructions = [
        7  %   3,
        99 %  98,
        13 %  49,
        39 %  35,
        36 %  91,
        10 % 143,
        49 %  13,
        7  %  11,
        1  %   2,
        91 %   1
    ]
}
