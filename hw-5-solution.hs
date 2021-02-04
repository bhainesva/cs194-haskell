import Data.Char
import Data.List (nub, maximumBy, intercalate, transpose, groupBy, genericLength, inits, tails, group)
import Data.Function (on)
import Data.Ord (comparing)

-- main: Run tests
main :: IO ()
main = putStr (formatTests testResults)

-- Exercise 1: Implement the following functions
-- 1.1
halveEvens :: [Integer] -> [Integer]
halveEvens = map (`div` 2) . filter even

ex_halveEvens =
    [ halveEvens [] == []
    , halveEvens [1,2,3,4,5] == [1,2]
    , halveEvens [6,6,6,3,3,3,2,2,2] == [3,3,3,1,1,1]
    ]

-- 1.2
safeString :: String -> String
safeString = map (\c -> if isControl c || not (isAscii c) then '_' else c)

-- safeString = map fix
--   where fix c | isControl c = '_'
--               | isAscii c   = c
--               | otherwise   = '_'

ex_safeString =
    [ safeString [] == []
    , safeString "Hello World!" == "Hello World!"
    , safeString "That’s your line:\n" == "That_s your line:_"
    , safeString "🙋.o(“Me Me Me”)" == "_.o(_Me Me Me_)"
    ]

-- 1.3
holes :: [a] -> [[a]]
holes = init . ((zipWith (\x y -> x ++ drop 1 y)) <$> inits <*> tails)

-- holes xs = zipWith (++) (inits xs) (map tail (init (tails xs)))

ex_holes =
   [ holes "" == []
   , holes "Hello" == ["ello", "Hllo", "Helo", "Helo", "Hell"]
   ]

-- 1.4
longestText :: Show a => [a] -> a
longestText = maximumBy (comparing (length . show))

ex_longestText =
   [ longestText [True,False] == False
   , longestText [2,4,16,32] == (32::Int)
   , longestText (words "Hello World") == "World"
   , longestText (words "Olá mundo") ==  "Olá"

   ]
-- 1.5
adjacents :: [a] -> [(a,a)]
adjacents (x:y:ys) = [(x,y)] ++ adjacents (y:ys)
adjacents _ = []

-- adjacents xs = zip xs (tail xs)

ex_adjacents =
   [ adjacents "" == []
   , adjacents [True] == []
   , adjacents "Hello" == [('H','e'),('e','l'),('l','l'),('l','o')]
   ]

-- 1.6
commas :: [String] -> String
commas = intercalate ", "

ex_commas =
   [ commas [] == ""
   , commas ["Hello"] == "Hello"
   , commas ["Hello", "World"] == "Hello, World"
   , commas ["Hello", "", "World"] == "Hello, , World"
   , commas ["Hello", "new", "World"] == "Hello, new, World"
   ]

-- 1.7
addPolynomials :: [[Integer]] -> [Integer]
addPolynomials = map sum . transpose

-- addPolynomials = foldl1 (zipWith (+))

ex_addPolynomials =
   [ addPolynomials [[]] == []
   , addPolynomials [[0, 1], [1, 1]] == [1, 2]
   , addPolynomials [[0, 1, 5], [7, 0, 0], [-2, -1, 5]] == [5, 0, 10]
   ]

--1.8
sumNumbers :: String -> Integer
sumNumbers = sum . map read . filter (all isNumber) . groupBy ((==) `on` isLetter)

-- sumNumbers = sum . map read . filter (isDigit.head) . groupBy ((==) `on` isLetter)

ex_sumNumbers =
   [ sumNumbers "" == 0
   , sumNumbers "Hello world!" == 0
   , sumNumbers "a1bc222d3f44" == 270
   , sumNumbers "words0are1234separated12by3integers45678" == 46927
   , sumNumbers "000a." == 0
   , sumNumbers "0.00a." == 0
   ]

-- Exercise 2: Implement wordCount w/ following output as spec
-- Number of lines: 23
-- Number of empty lines: 10
-- Number of words: 40
-- Number of unique words: 25
-- Number of words followed by themselves: 3
-- Length of the longest line: 5

wordCount :: String -> String
wordCount = unlines . sequence [numLines, numEmptyLines, numWords, numUniqWords, numRepeatedWords, longestLine]
   where
      numLines = ("Number of lines: "++) . show . length . lines
      numEmptyLines = ("Number of empty lines: "++) . show . length . filter null . lines
      numWords = ("Number of words: "++) . show . length . words
      numUniqWords = ("Number of unique words: "++) . show . length . nub . words
      numRepeatedWords = ("Number of words followed by themselves: "++) . show . sum . map ((1`subtract`) . length) . group . words
      longestLine = ("Length of the longest line: "++) . show . length . ((maximumBy . comparing) length) . lines

-- Tests
formatTests :: [(String, [Bool])] -> String
formatTests results = unlines (map
  (\(s, r) -> s ++ ": " ++ show (length (filter id r)) ++ "/" ++ show (length r) ++ " successful tests.") results)

testResults :: [(String, [Bool])]
testResults = [ ("halveEvens",      ex_halveEvens),
                ("safeString",      ex_safeString),
                ("holes",           ex_holes),
                ("longestText",     ex_longestText),
                ("adjacents",       ex_adjacents),
                ("commas",          ex_commas),
                ("addPolynomials",  ex_addPolynomials),
                ("sumNumbers",      ex_sumNumbers)
              ]