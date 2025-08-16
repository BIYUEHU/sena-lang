module Sudoku where

import Data.Maybe (mapMaybe)

type Grid = [[Int]]

-- Create an empty 9x9 grid
emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 0)

-- Safe list indexing returning Maybe
timeSafeIndex :: [a] -> Int -> Maybe a
timeSafeIndex [] _ = Nothing
timeSafeIndex (x : _) 0 = Just x
timeSafeIndex (_ : xs) n
  | n < 0 = Nothing
  | otherwise = timeSafeIndex xs (n - 1)

-- Get a row safely
getRow :: Grid -> Int -> Maybe [Int]
getRow = timeSafeIndex

-- Get a column safely
getCol :: Grid -> Int -> [Int]
getCol g c = mapMaybe (`timeSafeIndex` c) g

-- Get the 3x3 box containing (r,c)
getBox :: Grid -> (Int, Int) -> [Int]
getBox g (r, c) =
  let br = (r `div` 3) * 3
      bc = (c `div` 3) * 3
      collect [] = []
      collect ((dr, dc) : coords) =
        case timeSafeIndex g (br + dr) >>= (\row -> timeSafeIndex row (bc + dc)) of
          Just v -> v : collect coords
          Nothing -> collect coords
      allCoords = [(dr, dc) | dr <- [0 .. 2], dc <- [0 .. 2]]
   in collect allCoords

-- Check if placing n at (r,c) is valid
isValid :: Grid -> (Int, Int) -> Int -> Bool
isValid g (r, c) n =
  case getRow g r of
    Nothing -> False
    Just row ->
      notElem n row
        && notElem n (getCol g c)
        && notElem n (getBox g (r, c))

-- Find first empty cell safely
findEmpty :: Grid -> Maybe (Int, Int)
findEmpty g = findRow g 0
  where
    findRow [] _ = Nothing
    findRow (row : rows) r =
      case findCol row r 0 of
        Just c -> Just (r, c)
        Nothing -> findRow rows (r + 1)
    findCol [] _ _ = Nothing
    findCol (cell : cells) r c
      | cell == 0 = Just c
      | otherwise = findCol cells r (c + 1)

updateGrid :: Grid -> (Int, Int) -> Int -> Grid
updateGrid g (tr, tc) n = updateRows g 0
  where
    updateRows [] _ = []
    updateRows (row : rows) r =
      let newRow = if r == tr then updateCols row 0 else row
       in newRow : updateRows rows (r + 1)
    updateCols [] _ = []
    updateCols (cell : cells) c =
      let newCell = if r == tr && c == tc then n else cell
       in newCell : updateCols cells (c + 1)
      where
        r = tr -- ensure correct row index in scope

solve :: Grid -> Maybe Grid
solve g = case findEmpty g of
  Nothing -> Just g
  Just (r, c) -> tryNumbers [1 .. 9]
    where
      tryNumbers [] = Nothing
      tryNumbers (x : xs)
        | isValid g (r, c) x =
          let g' = updateGrid g (r, c) x
           in case solve g' of
                Just sol -> Just sol
                Nothing -> tryNumbers xs
        | otherwise = tryNumbers xs

generateFull :: Maybe Grid
generateFull = solve emptyGrid

digHoles :: Grid -> Int -> Grid
digHoles g 0 = g
digHoles g k = foldl removeCell g (take k (gen 0 0))
  where
    gen 9 _ = []
    gen r 9 = gen (r + 1) 0
    gen r c = (r, c) : gen r (c + 1)
    removeCell gr pos = updateGrid gr pos 0

generatePuzzle :: Int -> Maybe Grid
generatePuzzle clues = do
  full <- generateFull
  let removals = 81 - max 17 (min clues 81)
  return (digHoles full removals)

main :: IO ()
main = case generatePuzzle 30 of
  Nothing -> putStrLn "Failed to generate puzzle"
  Just puzzle -> do
    putStrLn "Puzzle:"
    mapM_ print puzzle
    case solve puzzle of
      Nothing -> putStrLn "No solution found"
      Just soln -> do
        putStrLn "Solution:"
        mapM_ print soln