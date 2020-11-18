module Text.Tabulate where

import Data.List

data Format = ColSep String | BoxDraw {
  horiz_vert :: Char,
  horiz_line :: Char,
  vert_line :: Char,
  down_left :: Char,
  down_right :: Char,
  up_left :: Char,
  up_right :: Char,
  horiz_up :: Char,
  horiz_down :: Char,
  vert_right :: Char,
  vert_left :: Char
}
  
defaultBoxDrawFmt = BoxDraw {
  horiz_vert = '┼',
  horiz_line = '─',
  vert_line = '│',
  down_left = '┐',
  down_right = '┌',
  up_right = '└',
  up_left = '┘',
  horiz_up = '┴',
  vert_left = '┤',
  vert_right = '├',
  horiz_down = '┬'
}

simpleFmt = ColSep " "


tabulate :: Format -> [[String]] -> String
tabulate (ColSep sep) cells =
  unlines $ map (intercalate sep) $ padCells cells
tabulate BoxDraw{..} cells = unlines $ map concat $ addVBorder $ addHBorder $ padCells cells where
  -- handling intersections on the edge makes this significantly more complex

  -- handle top intersections
  addVBorder (b:r:rs) =
    vBord down_right horiz_down down_left b:
    vLine r:addVBorder_ rs 
  
  -- handle bottom intersections
  addVBorder_ (b:[])  = vBord up_right horiz_up up_left b:[]
  addVBorder_ (b:r:rs) = vBord vert_right horiz_vert vert_left b:vLine r:addVBorder_ rs

  vBord l m r (xh:xt) = (l:xh):map (m:) xt++[[r]]
  vBord' b x = map (b:) x++[[b]]
  vLine = vBord' vert_line

  addHBorder [] = []
  addHBorder (r:[]) = hBord r:r:hBord r:[] -- this makes the last line a border
  addHBorder (r:rs) = hBord r:r:addHBorder rs

  hBord = map (map (const horiz_line))

getSizes :: [[[a]]] -> [Int]
getSizes = map (maximum . map length) . transpose

padCells :: [[String]] -> [[String]]
padCells cs = transpose pcs where
  pcs = [ map (padTo n) row | (n,row) <- zip sizes tcs ]
  sizes = map (foldr (max . length) 0) tcs
  tcs = transpose cs

padTo :: Int -> String -> String
padTo n s = take (n-length s) (repeat ' ')++s
