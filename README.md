# Haskell Maze Solver - Functional Programming Mini Project

## Group Members
- Dissanayake P.K - EG/2020/3916
- Diyunugala D.T.N - EG/2020/3918
- Ekanayake E.M.S.D - EG/2020/3922
- Rajapaksha R.D.K.S - EG/2020/4131

**Project Title:** Functional Maze Solver with Multiple Pathfinding Algorithms

---

## Problem Description

### Real-World Scenario
Maze solving is a fundamental problem in robotics, game AI, and navigation systems. This application demonstrates how functional programming can create reliable, safe pathfinding algorithms used in:

- **Autonomous Robots**: Navigating warehouses, hospitals, or disaster zones
- **Game AI**: Enemy pathfinding, NPC navigation in video games
- **GPS Systems**: Finding optimal routes in road networks
- **Circuit Design**: Routing connections on PCBs
- **Network Routing**: Finding paths in communication networks

Traditional imperative solutions suffer from:
- Mutable state bugs (changing visited flags incorrectly)
- Race conditions in parallel pathfinding
- Difficulty in testing and debugging stateful algorithms

Our **pure functional approach** ensures:
- ✓ **Correctness**: Pure functions guarantee same input → same output
- ✓ **Safety**: Immutable data structures prevent accidental modifications
- ✓ **Testability**: Each function can be tested in isolation
- ✓ **Parallelizability**: Pure functions can be safely parallelized

---

## Features

### Three Pathfinding Algorithms
1. **BFS (Breadth-First Search)**: Finds shortest path, explores level by level
2. **DFS (Depth-First Search)**: Quick exploration, uses less memory
3. **A\* (A-Star)**: Optimal pathfinding with heuristic guidance

### Functional Programming Concepts Used
- Pure functions (no side effects)
- Immutable data structures
- Recursion (no loops)
- Algebraic Data Types (ADTs)
- Pattern matching
- Higher-order functions
- Function composition
- Lazy evaluation

---

## Installation & Setup

### Prerequisites
- GHC (Glasgow Haskell Compiler) 8.10 or later
- GHCi (Interactive Haskell environment)

### Installing Haskell
**Windows:**
```bash
# Download from https://www.haskell.org/ghcup/
# Or use chocolatey:
choco install ghc
```

**Linux/Mac:**
```bash
curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | sh
```

---

## Running the Program

### Method 1: Using GHCi (Interactive)
```bash
# Navigate to project directory
cd "e:\FP_mini project"

# Launch GHCi and load Main module
ghci Main.hs

# Run the program
> main
```

### Method 2: Compile and Run
```bash
# Compile the program
ghc --make Main.hs -o MazeSolver

# Run the executable
./MazeSolver
```

### Method 3: Quick Run with runhaskell
```bash
runhaskell Main.hs
```

---

## Sample Input/Output

### Sample Maze File Format (`maze1.txt`)
```
##########
#S..#...#
###.#.##.#
#....#.#.#
#.###.#..#
#.....#..#
###.####.#
#........#
#.#####.E#
##########
```

**Legend:**
- `#` = Wall (impassable)
- `.` or space = Empty path
- `S` = Start position
- `E` = End/Goal position

### Example Session
```
╔══════════════════════════════════════╗
║     HASKELL MAZE SOLVER (FP)        ║
╚══════════════════════════════════════╝
1. Load maze from file
2. Use sample maze
3. Exit

Select option: 2

⏳ Loading sample maze...
✓ Maze loaded successfully!

--- Original Maze ---
╔════════════╗
║ ██████████████████ ║
║ ████S           ██ ║
║ ██████████    ████    ██ ║
║ ██              ████  ██ ║
...

╔══════════════════════════════════════╗
║    SELECT PATHFINDING ALGORITHM     ║
╚══════════════════════════════════════╝
1. BFS (Breadth-First Search) - Shortest Path
2. DFS (Depth-First Search) - Quick Exploration
3. A* (A-Star) - Optimal with Heuristic

Select algorithm: 1

⏳ Solving maze...
✓ Path found!
Path length: 23 steps

--- Solved Maze ---
[Maze with blue dots showing solution path]

╔══════════════════════════════════════╗
║         MAZE STATISTICS             ║
╚══════════════════════════════════════╝
Algorithm used: BFS
Maze dimensions: 10x10
Total cells: 100
Walls: 42
Empty spaces: 56
Wall ratio: 42%
Solution length: 23 cells
```

---

## Project Structure

```
FP_mini project/
├── Main.hs           # Entry point and main program flow
├── DataTypes.hs      # Algebraic Data Types (Cell, Maze, Position, etc.)
├── Processing.hs     # Pure pathfinding algorithms (BFS, DFS, A*)
├── IOHandler.hs      # Input/Output operations (file reading, display)
├── Utils.hs          # Utility functions (validation, neighbors, etc.)
├── README.md         # This file
├── TechnicalReport.md # Detailed technical documentation
├── maze1.txt         # Sample maze file (optional)
└── maze2.txt         # Another sample (optional)
```

---

## Functional Programming Concepts Demonstrated

### 1. **Pure Functions**
```haskell
-- No side effects, always returns same output for same input
manhattanDistance :: Position -> Position -> Int
manhattanDistance (x1, y1) (x2, y2) = abs (x1 - x2) + abs (y1 - y2)
```

### 2. **Immutability**
```haskell
-- Creates NEW maze with path, doesn't modify original
markPath :: Maze -> [Position] -> Maze
markPath maze path = maze { grid = markPathInGrid (grid maze) path }
```

### 3. **Recursion**
```haskell
-- BFS implemented recursively (no loops)
bfs :: [SearchState] -> [Position] -> PathResult
bfs [] _ = NoPath
bfs (state:rest) visited = ...
```

### 4. **Algebraic Data Types (ADT)**
```haskell
data Cell = Wall | Empty | Start | End | Path
data PathResult = PathFound [Position] | NoPath
```

### 5. **Pattern Matching**
```haskell
solveMaze "bfs" = solveMazeBFS
solveMaze "dfs" = solveMazeDFS
solveMaze "astar" = solveMazeAStar
solveMaze _ = solveMazeBFS
```

### 6. **Higher-Order Functions**
```haskell
-- filter, map, and function composition
getNeighbors :: Maze -> Position -> [Position]
getNeighbors maze (x, y) = filter (isValidPosition maze) candidates
```

---

## Creating Your Own Maze

Create a text file with the following format:

```
##########
#S......##
##.####..#
#...#....#
#.###.####
#.......E#
##########
```

**Rules:**
- Must have exactly ONE start `S` and ONE end `E`
- Use `#` for walls, `.` or space for empty paths
- Maze must be rectangular

---

## Testing the Code

### Test in GHCi
```haskell
-- Load modules
:load DataTypes Utils Processing

-- Test individual functions
> let pos1 = (0, 0)
> let pos2 = (3, 4)
> manhattanDistance pos1 pos2
7

> isValidPosition someMaze (1, 1)
True
```

---

## Expected Outputs

1. **Visual maze display** with color-coded cells
2. **Path visualization** showing solution route
3. **Statistics** including path length, algorithm used, maze dimensions
4. **Option to save** solution to file

---

## Why Functional Programming?

### Reliability
- **Pure functions** eliminate bugs from mutable state
- **Type safety** catches errors at compile time
- **Referential transparency** makes reasoning about code easier

### Concurrency
- **Immutable data** eliminates race conditions
- **Pure functions** can be parallelized safely
- Could extend to explore multiple paths simultaneously

### Maintainability
- **Modular design** - each module has single responsibility
- **Testable** - pure functions easy to unit test
- **Composable** - small functions combine to build complex behavior

---

## Possible Extensions

1. **Parallel Pathfinding**: Use multiple algorithms simultaneously
2. **Weighted Graphs**: Support different terrain costs
3. **3D Mazes**: Extend to three dimensions
4. **Visualization**: Animate the search process
5. **Maze Generation**: Create random solvable mazes
6. **Optimization**: Compare algorithm performance metrics
7. **Multiple Goals**: Find paths through multiple checkpoints

---

## Troubleshooting

**Issue**: `parse error on input 'module'`  
**Solution**: Check file encoding is UTF-8, no special characters

**Issue**: `Not in scope: type/data constructor`  
**Solution**: Ensure all modules are loaded: `:load Main.hs`

**Issue**: Colors not displaying correctly  
**Solution**: Use `displayMazeSimple` function for terminals without ANSI support

---


