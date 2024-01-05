module CreditCard
  ( toDigitsRev,
    toDigits,
    doubleEveryOther,
    testFunc,
    sumDigits,
    validate,
  )
where

toDigitsRev :: Integer -> [Integer]
toDigitsRev n
  | n <= 0 = []
  | otherwise = mod n 10 : toDigitsRev (div n 10)

toDigits :: Integer -> [Integer]
toDigits n
  | n <= 0 = []
  | otherwise = reverse (toDigitsRev n)

doubleEveryOtherEvenVersion :: [Integer] -> [Integer]
doubleEveryOtherEvenVersion [] = []
doubleEveryOtherEvenVersion [x] = [x]
doubleEveryOtherEvenVersion [x, y] = [x * 2, y]
doubleEveryOtherEvenVersion (x : y : xs) = x * 2 : y : doubleEveryOtherEvenVersion xs

doubleEveryOtherSingleVersion :: [Integer] -> [Integer]
doubleEveryOtherSingleVersion [] = []
doubleEveryOtherSingleVersion [x] = [x]
doubleEveryOtherSingleVersion [x, y] = [x * 2, y]
doubleEveryOtherSingleVersion (x : y : xs) = x : y * 2 : doubleEveryOtherEvenVersion xs

doubleEveryOther :: [Integer] -> [Integer]
doubleEveryOther [] = []
doubleEveryOther [x] = [x]
doubleEveryOther [x, y] = [x * 2, y]
doubleEveryOther xs
  | isLengthEven = doubleEveryOtherEvenVersion xs
  | otherwise = doubleEveryOtherSingleVersion xs
  where
    isLengthEven = even (length xs)

sumDigitsForInt :: Integer -> Integer
sumDigitsForInt n
  | n <= 0 = 0
  | otherwise = sum (toDigitsRev n)

sumDigits :: [Integer] -> Integer
sumDigits = sum . map sumDigitsForInt

validate :: Integer -> Bool
validate n = mod total 10 == 0
  where
    total = (sumDigits . doubleEveryOther . toDigits) n

testFunc :: Int -> String
testFunc 1 = "Hello"
testFunc _ = "World!"
