{-|
Module      : IOHandler
Description : Input/Output handling for the maze solver
Maintainer  : Your Group

Manages all side effects including file I/O, user interaction, and display.
Separates pure logic from impure operations following FP best practices.
-}

module IOHandler where

import DataTypes
import Utils
import Processing
import System.IO
import Control.Exception (catch, IOException)
import Data.Char (toLower)

-- | Reads a maze from a file
-- Format: # = Wall, . = Empty, S = Start, E = End
readMazeFromFile :: FilePath -> IO (Maybe Maze)
readMazeFromFile path = do
    result <- catch (readFile path >>= \content -> return (Just content))
                    (\e -> (do
                        putStrLn $ "Error reading file: " ++ show (e :: IOException)
                        return Nothing))
    case result of
        Nothing -> return Nothing
        Just content -> return $ parseMaze content

-- | Parses maze from string content
-- Pure parsing function that converts text to Maze structure
parseMaze :: String -> Maybe Maze
parseMaze content =
    let rows = lines content
        grid' = map parseLine rows
        h = length grid'
        w = if null grid' then 0 else length (head grid')
        startPos = findFirstCell Start grid'
        endPos = findFirstCell End grid'
    in case (startPos, endPos) of
        (Just s, Just e) -> Just $ Maze grid' w h s e
        _                -> Nothing

-- | Parses a single line of the maze
-- Pure function demonstrating map and pattern matching
parseLine :: String -> [Cell]
parseLine = map parseChar

-- | Converts character to Cell type
parseChar :: Char -> Cell
parseChar '#' = Wall
parseChar '.' = Empty
parseChar ' ' = Empty
parseChar 'S' = Start
parseChar 'E' = End
parseChar _   = Empty

-- | Displays maze in the terminal
-- Shows original maze or maze with solution path
displayMaze :: Maze -> IO ()
displayMaze maze = do
    putStrLn $ "\n+" ++ replicate (width maze * 2) '-' ++ "+"
    mapM_ displayRow (grid maze)
    putStrLn $ "+" ++ replicate (width maze * 2) '-' ++ "+\n"
    where
        displayRow :: [Cell] -> IO ()
        displayRow row = putStrLn $ "|" ++ concatMap cellToChar row ++ "|"

-- | Converts cell to display character with ANSI colors
cellToChar :: Cell -> String
cellToChar Wall  = "##"
cellToChar Empty = "  "
cellToChar Start = "S "
cellToChar End   = "E "
cellToChar Path  = "* "

-- | Displays maze without colors (for systems without ANSI support)
displayMazeSimple :: Maze -> IO ()
displayMazeSimple maze = do
    putStrLn $ "\n+" ++ replicate (width maze) '-' ++ "+"
    mapM_ (\row -> putStrLn $ "|" ++ map cellToCharSimple row ++ "|") (grid maze)
    putStrLn $ "+" ++ replicate (width maze) '-' ++ "+\n"

cellToCharSimple :: Cell -> Char
cellToCharSimple Wall  = '#'
cellToCharSimple Empty = ' '
cellToCharSimple Start = 'S'
cellToCharSimple End   = 'E'
cellToCharSimple Path  = '*'

-- | Interactive menu for user
showMenu :: IO ()
showMenu = do
    putStrLn "\n========================================"
    putStrLn "      HASKELL MAZE SOLVER (FP)        "
    putStrLn "========================================"
    putStrLn "1. Load maze from file"
    putStrLn "2. Use sample maze"
    putStrLn "3. Exit"
    putStr "\nSelect option: "
    hFlush stdout

-- | Algorithm selection menu
selectAlgorithm :: IO String
selectAlgorithm = do
    putStrLn "\n========================================"
    putStrLn "     SELECT PATHFINDING ALGORITHM     "
    putStrLn "========================================"
    putStrLn "1. BFS (Breadth-First Search) - Shortest Path"
    putStrLn "2. DFS (Depth-First Search) - Quick Exploration"
    putStrLn "3. A* (A-Star) - Optimal with Heuristic"
    putStr "\nSelect algorithm: "
    hFlush stdout
    choice <- getLine
    return $ case choice of
        "1" -> "bfs"
        "2" -> "dfs"
        "3" -> "astar"
        _   -> "bfs"

-- | Displays solution results
displaySolution :: PathResult -> IO ()
displaySolution NoPath = do
    putStrLn "\n[X] No path found! The maze is unsolvable."
displaySolution (PathFound path) = do
    putStrLn "\n[OK] Path found!"
    putStrLn $ "Path length: " ++ show (length path) ++ " steps"
    putStrLn $ "Steps: " ++ show (length path - 1)

-- | Creates a sample maze for demonstration
createSampleMaze :: Maze
createSampleMaze = Maze sampleGrid 10 10 (1, 1) (8, 8)
    where
        sampleGrid =
            [ [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
            , [Wall, Start, Empty, Empty, Wall, Empty, Empty, Empty, Empty, Wall]
            , [Wall, Wall, Wall, Empty, Wall, Empty, Wall, Wall, Empty, Wall]
            , [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall]
            , [Wall, Empty, Wall, Wall, Wall, Wall, Empty, Wall, Empty, Wall]
            , [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Wall, Empty, Wall]
            , [Wall, Wall, Wall, Empty, Wall, Wall, Wall, Wall, Empty, Wall]
            , [Wall, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Empty, Wall]
            , [Wall, Empty, Wall, Wall, Wall, Wall, Wall, Wall, End, Wall]
            , [Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall, Wall]
            ]

-- | Saves the solved maze to a file
saveSolution :: FilePath -> Maze -> IO ()
saveSolution path maze = do
    let content = unlines $ map (map cellToCharSimple) (grid maze)
    writeFile path content
    putStrLn $ "\n[OK] Solution saved to: " ++ path

-- | Displays maze statistics
displayStatistics :: Maze -> PathResult -> String -> IO ()
displayStatistics maze result algorithm = do
    let (total, walls, empty, ratio) = analyzeMaze maze
    putStrLn "\n========================================"
    putStrLn "          MAZE STATISTICS             "
    putStrLn "========================================"
    putStrLn $ "Algorithm used: " ++ algorithm
    putStrLn $ "Maze dimensions: " ++ show (width maze) ++ "x" ++ show (height maze)
    putStrLn $ "Total cells: " ++ show total
    putStrLn $ "Walls: " ++ show walls
    putStrLn $ "Empty spaces: " ++ show empty
    putStrLn $ "Wall ratio: " ++ show (round (ratio * 100)) ++ "%"
    case result of
        PathFound path -> putStrLn $ "Solution length: " ++ show (length path) ++ " cells"
        NoPath -> putStrLn "Solution: None found"
