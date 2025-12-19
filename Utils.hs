{-|
Module      : Utils
Description : Utility functions for maze operations
Maintainer  : Your Group

Pure utility functions for maze manipulation, validation, and common operations.
All functions are referentially transparent and side-effect free.
-}

module Utils where

import DataTypes

-- | Gets the cell at a specific position in the maze
-- Pure function: same input always produces same output
getCellAt :: Maze -> Position -> Maybe Cell
getCellAt maze (x, y)
    | x < 0 || y < 0 || x >= width maze || y >= height maze = Nothing
    | otherwise = Just ((grid maze !! y) !! x)

-- | Checks if a position is valid (within bounds and not a wall)
isValidPosition :: Maze -> Position -> Bool
isValidPosition maze pos =
    case getCellAt maze pos of
        Just Wall -> False
        Just _    -> True
        Nothing   -> False

-- | Gets all valid neighboring positions (up, down, left, right)
-- Demonstrates pure function composition and filtering
getNeighbors :: Maze -> Position -> [Position]
getNeighbors maze (x, y) =
    filter (isValidPosition maze) candidates
    where
        candidates = [(x, y-1), (x, y+1), (x-1, y), (x+1, y)]

-- | Checks if a position has been visited
-- Uses list membership - immutable data structure
isVisited :: Position -> [Position] -> Bool
isVisited = elem

-- | Converts a direction to a position delta
directionToDelta :: Direction -> (Int, Int)
directionToDelta North = (0, -1)
directionToDelta South = (0, 1)
directionToDelta East  = (1, 0)
directionToDelta West  = (-1, 0)

-- | Applies a direction to a position
-- Pure transformation function
move :: Position -> Direction -> Position
move (x, y) dir =
    let (dx, dy) = directionToDelta dir
    in (x + dx, y + dy)

-- | Calculates Manhattan distance between two positions
-- Heuristic function for A* algorithm
manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)

-- | Marks a path on the maze (creates new maze with path marked)
-- Demonstrates immutability - returns new maze instead of modifying
markPath :: Maze -> [Position] -> Maze
markPath maze path =
    maze { grid = markPathInGrid (grid maze) path }

-- | Helper function to mark path in grid (recursive)
markPathInGrid :: [[Cell]] -> [Position] -> [[Cell]]
markPathInGrid g [] = g
markPathInGrid g path =
    updateGrid g path
    where
        updateGrid :: [[Cell]] -> [Position] -> [[Cell]]
        updateGrid grid' [] = grid'
        updateGrid grid' ((x, y):ps) =
            let row = grid' !! y
                newRow = take x row ++ [Path] ++ drop (x + 1) row
                newGrid = take y grid' ++ [newRow] ++ drop (y + 1) grid'
            in updateGrid newGrid ps

-- | Validates that a maze has exactly one start and one end
validateMaze :: Maze -> Bool
validateMaze maze =
    let cells = concat (grid maze)
        startCount = length $ filter (== Start) cells
        endCount = length $ filter (== End) cells
    in startCount == 1 && endCount == 1

-- | Finds positions of specific cell type (used to locate start/end)
-- Higher-order function demonstrating function composition
findCell :: Cell -> [[Cell]] -> [Position]
findCell cellType grid' =
    [(x, y) | (y, row) <- zip [0..] grid',
              (x, cell) <- zip [0..] row,
              cell == cellType]

-- | Gets the first occurrence of a cell type
findFirstCell :: Cell -> [[Cell]] -> Maybe Position
findFirstCell cellType grid' =
    case findCell cellType grid' of
        []    -> Nothing
        (p:_) -> Just p
