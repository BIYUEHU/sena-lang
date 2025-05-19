-- Sudoku Generator and Solver in Haskell with Safe Indexing

import Data.Maybe (mapMaybe, maybeToList)

type Grid = [[Int]]

-- Create an empty 9x9 grid
emptyGrid :: Grid
emptyGrid = replicate 9 (replicate 9 0)

-- Safe list indexing returning Maybe
timeSafeIndex :: [a] -> Int -> Maybe a

imeSafeIndex [] _ = Nothing

timeSafeIndex (x : _) 0 = Just x
timeSafeIndex (_ : xs) n
  | n < 0 = Nothing
  | otherwise = timeSafeIndex xs (n - 1)

-- Get a row safely
getRow :: Grid -> Int -> Maybe [Int]
getRow g r = timeSafeIndex g r

-- Get a column safely
getCol :: Grid -> Int -> [Int]
getCol g c = mapMaybe (\row -> timeSafeIndex row c) g

-- Get the 3x3 box containing (r,c)
getBox :: Grid -> (Int, Int) -> [Int]
getBox g (r, c) =
  let br = (r `div` 3) * 3
      bc = (c `div` 3) * 3
   in concatMap
        ( \dr ->
            concatMap
              ( \dc ->
                  maybeToList $ do
                    row <- timeSafeIndex g (br + dr)
                    timeSafeIndex row (bc + dc)
              )
              [0 .. 2]
        )
        [0 .. 2]

-- Check if placing n at (r,c) is valid
isValid :: Grid -> (Int, Int) -> Int -> Bool
isValid g (r, c) n =
  case getRow g r of
    Nothing -> False
    Just row ->
      notElem n row
        && notElem n (getCol g c)
        && notElem n (getBox g (r, c))

-- Find first empty cell; returns Nothing if none
findEmpty :: Grid -> Maybe (Int, Int)
findEmpty g = findInCoords coords
  where
    coords = [(r, c) | r <- [0 .. 8], c <- [0 .. 8]]
    findInCoords [] = Nothing
    findInCoords ((r, c) : cs) =
      case timeSafeIndex g r >>= (\row -> timeSafeIndex row c) of
        Just 0 -> Just (r, c)
        _ -> findInCoords cs

-- Update grid at position with a number
updateGrid :: Grid -> (Int, Int) -> Int -> Grid
updateGrid g (r, c) n =
  [ if i == r
      then [if j == c then n else cell | (j, cell) <- zip [0 ..] row]
      else row
    | (i, row) <- zip [0 ..] g
  ]

-- Backtracking solver: returns a solved grid or Nothing
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

-- Generate a complete filled grid
generateFull :: Maybe Grid
generateFull = solve emptyGrid

-- Remove k cells to create a puzzle (simple non-unique strategy)
digHoles :: Grid -> Int -> Grid
digHoles g 0 = g
digHoles g k =
  let coords = [(r, c) | r <- [0 .. 8], c <- [0 .. 8]]
      toRemove = take k coords
   in foldl (\gr pos -> updateGrid gr pos 0) g toRemove

-- Generate a puzzle with given number of clues
generatePuzzle :: Int -> Maybe Grid
generatePuzzle clues = do
  full <- generateFull
  let removals = 81 - max 17 (min clues 81)
  return (digHoles full removals)

-- Example main to print puzzle and solution
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
