{-# LANGUAGE BlockArguments #-}

module Sudoku where
import Data.Char
import Data.List
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
allBlankSudoku = Sudoku [[Nothing | _ <- [1..9]] | _ <- [1..9]]

-- * A2

isSudoku :: Sudoku -> Bool
isSudoku (Sudoku rows) = (length rows) == 9 && checkRows rows
  where
    checkRows []                                      = True
    checkRows (currentRow:restOfRows)                 = (length currentRow) == 9 && checkCells currentRow && checkRows restOfRows
    checkCells []                                     = True
    checkCells (Nothing:restOfCells)                  = checkCells restOfCells
    checkCells (Just n:restOfCells) | n > 0 && n < 10 = checkCells restOfCells
                                    | otherwise       = False 

-- * A3

isFilled :: Sudoku -> Bool
isFilled (Sudoku rows)= checkFilledRows rows
  where
    checkFilledRows []                      = True
    checkFilledRows (currentRow:restOfRows) = checkFilledCells currentRow && checkFilledRows restOfRows
    checkFilledCells []                     = True
    checkFilledCells (Nothing:restOfCells)  = False
    checkFilledCells (Just _:restOfCells)   = checkFilledCells restOfCells

------------------------------------------------------------------------------

-- * B1

-- | printSudoku sud prints a nice representation of the sudoku sud on
-- the screen
printSudoku :: Sudoku -> IO ()
printSudoku (Sudoku rows) = putStr (printRows rows)
  where
    printRows [] = ""
    printRows (currentRow:restOfRows) = printCells currentRow ++ printRows restOfRows
    printCells [] = "\n"
    printCells (Nothing:restOfCells) = "." ++ printCells restOfCells
    printCells (Just n:restOfCells) = show n ++ printCells restOfCells

-- * B2

-- | readSudoku file reads from the file, and either delivers it, or stops
-- if the file did not contain a sudoku
readSudoku :: FilePath -> IO Sudoku
readSudoku filePath = do 
  s <- readFile filePath
  let sudoku = sudokuFromString s
  if isSudoku sudoku then return sudoku else error "Invalid sudoku format"

sudokuFromString :: String -> Sudoku
sudokuFromString sudokuString = Sudoku $ rowsFromStrings $ lines sudokuString
  where
    rowsFromStrings :: [String] -> [Row]
    rowsFromStrings [] = []
    rowsFromStrings (currentString:restOfStrings) = cellsFromString currentString : rowsFromStrings restOfStrings 
    cellsFromString :: String -> [Cell]
    cellsFromString [] = []
    cellsFromString (currentChar:restOfChars) | currentChar == '.' = Nothing : cellsFromString restOfChars
    cellsFromString (currentChar:restOfChars)                      = Just (digitToInt currentChar) : cellsFromString restOfChars

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
isOkayBlock block = length d == length (nub d)
  where
    d                             = digits block
    digits []                     = []
    digits (Nothing:restOfCells)  = digits restOfCells
    digits ((Just n):restOfCells) = n : digits restOfCells


-- * D2

square :: [Row] -> Integer -> Integer -> Block
square rows x y = (take 3 $ drop (3 * xi) $ rows !! (0 + 3 * yi)) ++ 
                  (take 3 $ drop (3 * xi) $ rows !! (1 + 3 * yi)) ++ 
                  (take 3 $ drop (3 * xi) $ rows !! (2 + 3 * yi))
  where
    xi = fromIntegral x
    yi = fromIntegral y

blocks :: Sudoku -> [Block]
blocks (Sudoku rows) = [square rows r c | r <- [0..2], c <- [0..2]]    

prop_blocks_lengths :: Sudoku -> Bool
prop_blocks_lengths sudoku = length bs == 9 && and [length b == 9 | b <- bs]
  where
    bs = blocks sudoku  

-- * D3

isOkayLine :: Row -> Bool
isOkayLine line = length d == length (nub d)
  where
    d                             = digits line
    digits []                     = []
    digits (Nothing:restOfCells)  = digits restOfCells
    digits ((Just n):restOfCells) = n : digits restOfCells

isOkay :: Sudoku -> Bool
isOkay (Sudoku rows) = 
  isSudoku (Sudoku rows) && 
  and [isOkayBlock b | b <- blocks (Sudoku rows)] && 
  and [isOkayLine line | line <- rows] &&
  and [isOkayLine line | line <- (transpose rows)]

---- Part A ends here --------------------------------------------------------
------------------------------------------------------------------------------
---- Part B starts here ------------------------------------------------------


-- | Positions are pairs (row,column),
-- (0,0) is top left corner, (8,8) is bottom left corner
type Pos = (Int,Int)

-- * E1

blanks :: Sudoku -> [Pos]
blanks (Sudoku r) = rows r 0
  where
    rows :: [Row] -> Int -> [Pos]
    rows []                      _   = []
    rows (currentRow:restOfRows) row = cells currentRow row 0 ++ rows restOfRows (row + 1)
    cells :: Row -> Int -> Int -> [Pos]
    cells []                    _   _   = []
    cells (Nothing:restOfCells) row col = (row, col) : cells restOfCells row (col + 1)
    cells (Just _:restOfCells)  row col = cells restOfCells row (col + 1)

prop_blanks_allBlanks :: Bool
prop_blanks_allBlanks = (length $ blanks allBlankSudoku) == 81


-- * E2

(!!=) :: [a] -> (Int, a) -> [a]
[]                    !!= (_,     _)     = []
(currentEl:restOfEls) !!= (0,     newEl) = newEl : restOfEls
(currentEl:restOfEls) !!= (index, newEl) = currentEl : (restOfEls !!= (index - 1, newEl))

prop_bangBangEquals_correct :: [Integer] -> (Int, Integer) -> Bool
prop_bangBangEquals_correct els (index, newEl) 
  | index < 0           = els == (els !!= (index, newEl)) -- index out of range, nothing should change
  | index >= length els = els == (els !!= (index, newEl)) -- index out of range, nothing should change
  | otherwise           = take index els == take index newEls && -- preceeding elements are the same
                          newEls !! index == newEl && -- change element is the new elements in the resulting list
                          drop (index + 1) els == drop (index + 1) newEls -- succeeding elements are the same
  where
    newEls = els !!= (index, newEl)


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
