module Sudoku where
import Data.Char
import Data.List
import Data.Maybe
import Test.QuickCheck

------------------------------------------------------------------------------

-- | Representation of sudoku puzzles (allows some junk)
type Cell = Maybe Int -- a single cell
type Row  = [Cell]    -- a row is a list of cells

data Sudoku = Sudoku [Row] 
 deriving ( Show, Eq )

rows :: Sudoku -> [Row]
rows (Sudoku ms) = ms

-- | A sample sudoku puzzle
example :: Sudoku
example =
    Sudoku
      [ [j 3,j 6,n  ,n  ,j 7,j 1,j 2,n  ,n  ]
      , [n  ,j 5,n  ,n  ,n  ,n  ,j 1,j 8,n  ]
      , [n  ,n  ,j 9,j 2,n  ,j 4,j 7,n  ,n  ]
      , [n  ,n  ,n  ,n  ,j 1,j 3,n  ,j 2,j 8]
      , [j 4,n  ,n  ,j 5,n  ,j 2,n  ,n  ,j 9]
      , [j 2,j 7,n  ,j 4,j 6,n  ,n  ,n  ,n  ]
      , [n  ,n  ,j 5,j 3,n  ,j 8,j 9,n  ,n  ]
      , [n  ,j 8,j 3,n  ,n  ,n  ,n  ,j 6,n  ]
      , [n  ,n  ,j 7,j 6,j 9,n  ,n  ,j 4,j 3]
      ]
  where
    n = Nothing
    j = Just

-- * A1

allBlankSudoku :: Sudoku
allBlankSudoku = Sudoku $ replicate 9 $ replicate 9 Nothing
-- * A2

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = (length rows) == 9 && all checkRow rows
  where
    checkRow row                       = (length row) == 9 && all checkCell row
    checkCell Nothing                  = True
    checkCell (Just n)                 = n > 0 && n < 10

-- * A3

isFilled :: Sudoku -> Bool
isFilled (Sudoku rows) = all checkRow rows
  where
    checkRow row       = all checkCell row
    checkCell Nothing  = False
    checkCell (Just _) = True

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = putStr $ concat (map printRow rows)
    where
      printRow row = (unlines $ map printCell row) ++ "\n"
      printCell Nothing = "."
      printCell (Just n) = show n
-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do 
  s <- readFile filePath
  let sudoku = sudokuFromString s
  if isSudoku sudoku then return sudoku else error "Invalid sudoku format"

sudokuFromString :: String -> Sudoku
sudokuFromString sudokuString = Sudoku $ map rowsFromLine $ lines sudokuString
  where
    rowsFromLine line = map cellFromString line
    cellFromString string = if string == '.' then Nothing else Just (digitToInt string)

------------------------------------------------------------------------------

-- * C1

-- | cell generates an arbitrary cell in a Sudoku
cell :: Gen (Cell)
cell = frequency [(9, nothings), (1, justs)]
  where
    nothings = elements [Nothing]
    justs    = elements [Just n | n <- [1..9]]

-- * C2

-- | an instance for generating Arbitrary Sudokus
instance Arbitrary Sudoku where
  arbitrary = do
    rows <- vectorOf 9 $ vectorOf 9 cell
    return $ Sudoku rows

printRandomSudoku :: IO ()
printRandomSudoku = do
  sud <- kek
  printSudoku sud
  where
    kek :: IO Sudoku
    kek = generate arbitrary

 -- hint: get to know the QuickCheck function vectorOf
 
-- * C3

prop_Sudoku :: Sudoku -> Bool
prop_Sudoku = isSudoku
  -- hint: this definition is simple!
  
------------------------------------------------------------------------------

type Block = [Cell] -- a Row is also a Cell


-- * D1

isOkayBlock :: Block -> Bool
isOkayBlock block = length digits == length (nub digits)
  where
    digits = filter (\cell -> isJust cell) block

-- * D2

square :: [Row] -> Integer -> Integer -> Block
square rows x y = (take 3 $ drop (3 * xi) $ rows !! (0 + 3 * yi)) ++ 
                  (take 3 $ drop (3 * xi) $ rows !! (1 + 3 * yi)) ++ 
                  (take 3 $ drop (3 * xi) $ rows !! (2 + 3 * yi))
  where
    xi = fromIntegral x
    yi = fromIntegral y


blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = squares ++ rows ++ columns
  where
    squares = [square rows r c | r <- [0..2], c <- [0..2]]
    columns = transpose rows

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = length bs == 27 && and [length b == 9 | b <- bs]
  where
    bs = blocks sudoku

-- * D3

isOkay :: Sudoku -> Bool
isOkay (Sudoku rows) = 
  isSudoku (Sudoku rows) && 
  and [isOkayBlock b | b <- blocks (Sudoku rows)]

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks = undefined

--prop_blanks_allBlanks :: ...
--prop_blanks_allBlanks =


-- * E2

(!!=) :: [a] -> (Int,a) -> [a]
xs !!= (i,y) = undefined

--prop_bangBangEquals_correct :: ...
--prop_bangBangEquals_correct =


-- * E3

update :: Sudoku -> Pos -> Cell -> Sudoku
update = undefined

--prop_update_updated :: ...
--prop_update_updated =


------------------------------------------------------------------------------

-- * F1


-- * F2


-- * F3


-- * F4
