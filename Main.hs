{-|
Module      : Main
Description : Entry point for the Maze Solver application
Maintainer  : Your Group

Main program flow orchestrating the maze solver with user interaction.
Demonstrates separation of pure logic and side effects.
-}

module Main where

import DataTypes
import Processing
import IOHandler
import Utils
import System.IO

-- | Main entry point - orchestrates the application flow
main :: IO ()
main = do
    hSetBuffering stdout NoBuffering
    mainLoop

-- | Main application loop
-- Handles user interaction and program flow
mainLoop :: IO ()
mainLoop = do
    showMenu
    choice <- getLine
    case choice of
        "1" -> loadAndSolveMaze
        "2" -> solveSampleMaze
        "3" -> do
            putStrLn "\nThank you for using Haskell Maze Solver!"
            putStrLn "Demonstrating Functional Programming principles:"
            putStrLn "  [OK] Pure Functions"
            putStrLn "  [OK] Immutability"
            putStrLn "  [OK] Recursion"
            putStrLn "  [OK] Algebraic Data Types"
            putStrLn "  [OK] Pattern Matching"
        _   -> do
            putStrLn "\n[X] Invalid choice. Please try again."
            mainLoop

-- | Loads maze from file and solves it
loadAndSolveMaze :: IO ()
loadAndSolveMaze = do
    putStr "\nEnter maze file path: "
    path <- getLine
    putStrLn "\n[...] Loading maze..."
    maybeMaze <- readMazeFromFile path
    case maybeMaze of
        Nothing -> do
            putStrLn "[X] Failed to load maze. Please check the file format."
            putStrLn "\nExpected format:"
            putStrLn "  # = Wall"
            putStrLn "  . or space = Empty"
            putStrLn "  S = Start"
            putStrLn "  E = End"
            mainLoop
        Just maze -> do
            if validateMaze maze
                then processMaze maze
                else do
                    putStrLn "[X] Invalid maze: Must have exactly one Start (S) and one End (E)"
                    mainLoop

-- | Solves the sample maze
solveSampleMaze :: IO ()
solveSampleMaze = do
    putStrLn "\n[...] Loading sample maze..."
    let maze = createSampleMaze
    processMaze maze

-- | Processes and solves a given maze
-- Demonstrates pure/impure separation in FP
processMaze :: Maze -> IO ()
processMaze maze = do
    putStrLn "\n[OK] Maze loaded successfully!"
    putStrLn "\n--- Original Maze ---"
    displayMaze maze
    
    -- Select algorithm
    algorithm <- selectAlgorithm
    
    -- Pure computation - solving the maze
    putStrLn "\n[...] Solving maze..."
    let result = solveMaze algorithm maze
    
    -- Display results
    displaySolution result
    
    case result of
        PathFound path -> do
            -- Pure transformation - marking path on maze
            let solvedMaze = markPath maze path
            putStrLn "\n--- Solved Maze ---"
            displayMaze solvedMaze
            
            -- Statistics
            displayStatistics solvedMaze result (map toUpper algorithm)
            
            -- Option to save
            putStr "\nSave solution to file? (y/n): "
            save <- getLine
            when (save == "y" || save == "Y") $ do
                putStr "Enter output file path: "
                outPath <- getLine
                saveSolution outPath solvedMaze
        
        NoPath -> do
            displayStatistics maze result (map toUpper algorithm)
    
    -- Continue or exit
    putStr "\nPress Enter to continue..."
    _ <- getLine
    mainLoop

-- | Helper to convert string to uppercase
toUpper :: Char -> Char
toUpper c = if c >= 'a' && c <= 'z' then toEnum (fromEnum c - 32) else c

-- | Helper for when condition
when :: Bool -> IO () -> IO ()
when True action = action
when False _     = return ()
