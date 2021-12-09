
module Misc (check,readInts,collate) where

import qualified Data.Map as Map

check :: (Eq a, Show a) => a -> a -> a
check x y = if x == y then x else error (show (x,y))

readInts :: FilePath -> IO [Int]
readInts path = do
  str <- readFile path
  pure $ map read (words str)

collate :: Ord k => [(k,v)] -> [(k,[v])]
collate xs = Map.toList (Map.fromListWith (++) [ (k,[v]) | (k,v) <- xs ])
