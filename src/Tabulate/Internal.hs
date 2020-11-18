module Text.Tabulate.Internal where

getSizes :: [[[a]]] -> [Int]
getSizes = map (maximum . map length) . transpose

padCells :: [[String]] -> [[String]]
padCells cs = transpose pcs where
  pcs = [ map (padTo n) row | (n,row) <- zip sizes tcs ]
  sizes = map (foldr (max . length) 0) tcs
  tcs = transpose cs

padTo :: Int -> String -> String
padTo n s = take (n-length s) (repeat ' ')++s
