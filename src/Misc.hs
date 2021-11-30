
module Misc (check,readInts) where

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

readInts :: FilePath -> IO [Int]
readInts path = do
  str <- readFile path
  pure $ map read (words str)
