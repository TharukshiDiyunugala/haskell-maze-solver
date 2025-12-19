{-|
Module      : Processing
Description : Core pathfinding algorithms for maze solving
Maintainer  : Your Group

Implements pure functional pathfinding algorithms including BFS and DFS.
All algorithms are implemented using recursion and immutable data structures.
-}

module Processing where

import DataTypes
import Utils
import Data.List (find)

-- | Breadth-First Search (BFS) - finds shortest path
-- Pure recursive implementation using queue-like behavior
solveMazeBFS :: Maze -> PathResult
solveMazeBFS maze = bfs [initialState] []
    where
        initialState = SearchState (start maze) [] [start maze]
        goalPos = end maze

        -- Recursive BFS implementation
        -- Base case: no more states to explore
        bfs :: [SearchState] -> [Position] -> PathResult
        bfs [] _ = NoPath
        
        -- Check if current state reaches the goal
        bfs (state:rest) visitedSet
            | current state == goalPos = PathFound (reverse (pathSoFar state))
            | current state `elem` visitedSet = bfs rest visitedSet
            | otherwise =
                let newVisited = current state : visitedSet
                    neighbors = getNeighbors maze (current state)
                    unvisitedNeighbors = filter (`notElem` newVisited) neighbors
                    newStates = map (createState state) unvisitedNeighbors
                in bfs (rest ++ newStates) newVisited

        createState :: SearchState -> Position -> SearchState
        createState parent pos = SearchState pos
                                              (current parent : visited parent)
                                              (pos : pathSoFar parent)

-- | Depth-First Search (DFS) - explores deeply before backtracking
-- Demonstrates recursive backtracking algorithm
solveMazeDFS :: Maze -> PathResult
solveMazeDFS maze = dfs (start maze) [] [start maze]
    where
        goalPos = end maze

        -- Recursive DFS with backtracking
        dfs :: Position -> [Position] -> [Position] -> PathResult
        dfs currentPos visitedSet path
            | currentPos == goalPos = PathFound (reverse path)
            | currentPos `elem` visitedSet = NoPath
            | otherwise =
                let newVisited = currentPos : visitedSet
                    neighbors = getNeighbors maze currentPos
                    unvisitedNeighbors = filter (`notElem` newVisited) neighbors
                in exploreNeighbors unvisitedNeighbors newVisited path

        -- Try each neighbor recursively (backtracking)
        exploreNeighbors :: [Position] -> [Position] -> [Position] -> PathResult
        exploreNeighbors [] _ _ = NoPath
        exploreNeighbors (n:ns) visited path =
            case dfs n visited (n:path) of
                PathFound p -> PathFound p
                NoPath      -> exploreNeighbors ns visited path

-- | A* Search - uses heuristic for optimal pathfinding
-- More efficient than BFS for large mazes
solveMazeAStar :: Maze -> PathResult
solveMazeAStar maze = astar [initialState] []
    where
        initialState = (0, SearchState (start maze) [] [start maze])
        goalPos = end maze

        -- A* with priority queue (simulated using list sorting)
        astar :: [(Int, SearchState)] -> [Position] -> PathResult
        astar [] _ = NoPath
        astar states visitedSet =
            let sorted = sortByPriority states
                ((_, state):rest) = sorted
                pos = current state
            in if pos == goalPos
               then PathFound (reverse (pathSoFar state))
               else if pos `elem` visitedSet
                    then astar rest visitedSet
                    else let newVisited = pos : visitedSet
                             neighbors = getNeighbors maze pos
                             unvisitedNeighbors = filter (`notElem` newVisited) neighbors
                             newStates = map (createAStarState state) unvisitedNeighbors
                         in astar (rest ++ newStates) newVisited

        createAStarState :: SearchState -> Position -> (Int, SearchState)
        createAStarState parent pos =
            let newPath = pos : pathSoFar parent
                gCost = length newPath
                hCost = manhattanDistance pos goalPos
                fCost = gCost + hCost
                newState = SearchState pos (current parent : visited parent) newPath
            in (fCost, newState)

        sortByPriority :: [(Int, SearchState)] -> [(Int, SearchState)]
        sortByPriority [] = []
        sortByPriority (x:xs) =
            let smaller = sortByPriority [a | a <- xs, fst a <= fst x]
                larger = sortByPriority [a | a <- xs, fst a > fst x]
            in smaller ++ [x] ++ larger

-- | Solves maze using specified algorithm
-- Demonstrates pattern matching and algebraic data types
solveMaze :: String -> Maze -> PathResult
solveMaze "bfs" = solveMazeBFS
solveMaze "dfs" = solveMazeDFS
solveMaze "astar" = solveMazeAStar
solveMaze _ = solveMazeBFS  -- Default to BFS

-- | Analyzes the complexity of a maze
-- Pure function that computes maze statistics
analyzeMaze :: Maze -> (Int, Int, Int, Double)
analyzeMaze maze =
    let cells = concat (grid maze)
        totalCells = length cells
        wallCount = length $ filter (== Wall) cells
        emptyCount = length $ filter (== Empty) cells
        wallRatio = fromIntegral wallCount / fromIntegral totalCells :: Double
    in (totalCells, wallCount, emptyCount, wallRatio)
