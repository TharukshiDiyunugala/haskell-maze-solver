{-|
Module      : DataTypes
Description : Core data types and structures for the Maze Solver
Maintainer  : Your Group

This module defines all the algebraic data types (ADTs) used in the maze solver,
including Position, Cell, Maze, and Path representations.
-}

module DataTypes where

-- | Represents a 2D position in the maze using Cartesian coordinates
type Position = (Int, Int)

-- | Algebraic Data Type representing different cell types in a maze
data Cell = Wall      -- ^ Impassable wall cell
          | Empty     -- ^ Traversable empty cell
          | Start     -- ^ Starting position
          | End       -- ^ Goal/destination position
          | Path      -- ^ Cell that's part of the solution path
          deriving (Eq, Show)

-- | Direction type for maze traversal
data Direction = North | South | East | West
               deriving (Eq, Show, Enum, Bounded)

-- | Maze represented as a 2D list of Cells with dimensions
data Maze = Maze {
    grid   :: [[Cell]],     -- ^ 2D grid of cells
    width  :: Int,          -- ^ Width of the maze
    height :: Int,          -- ^ Height of the maze
    start  :: Position,     -- ^ Starting position coordinates
    end    :: Position      -- ^ Goal position coordinates
} deriving (Show)

-- | Result of pathfinding algorithm
data PathResult = PathFound [Position]   -- ^ Successful path with list of positions
                | NoPath                  -- ^ No solution exists
                deriving (Eq, Show)

-- | Search state for pathfinding algorithms (used internally)
data SearchState = SearchState {
    current   :: Position,        -- ^ Current position being explored
    visited   :: [Position],      -- ^ List of visited positions
    pathSoFar :: [Position]       -- ^ Path taken to reach current position
} deriving (Show)
