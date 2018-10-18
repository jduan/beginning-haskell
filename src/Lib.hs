module Lib where

someFunc :: IO ()
someFunc = putStrLn "someFunc"

-- firstOrEmpty :: [a] -> a
firstOrEmpty xs =
  if not (null xs)
    then head xs
    else "empty"

xs +++ ys =
  if null xs
    then ys
    else head xs : tail xs +++ ys

reverse2 :: [a] -> [a]
reverse2 xs = go xs []
  where
    go [] ys = ys
    go (x:xs) ys = go xs (x : ys)

maxmin :: Ord a => [a] -> (a, a)
maxmin [] = error "need at least 1 element"
maxmin [x] = (x, x)
maxmin (x:xs) = (large', small')
  where
    (large, small) = maxmin xs
    large' = max large x
    small' = min small x

data Client
  = GovOrg String
  | Company String
            Integer
            String
            String
  | Individual Person
               Bool
  deriving (Show, Eq)

data Person =
  Person String
         String
         Gender
  deriving (Show, Eq)

data Gender
  = Male
  | Female
  | Unknown
  deriving (Show, Eq)

client = Individual (Person "Jack" "Smith" Male)
