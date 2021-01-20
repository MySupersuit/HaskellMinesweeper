module Minesweeper 
    ( ApparentGrid
    , ActualGrid
    , Coord
    , initGame
    , handleInput
    , checkEndgame
    , getNumFlags
    , getNumMines
    , getNumUnopened
    , flagAllUnmarked
    , unopenedAdjCoords
    , getOpenedAdjCoords
    , flagMultiple
    , height
    , width
    , (@!!)
    , numAdjFlags
    , openMultipleSquares
    , openZero
    , openedAdjCoords
    , getMineCoords
    , getFlaggedCoords
    )
where

import System.Random
import Data.Time.Clock.POSIX
import Data.List
import Control.Monad

-------------------------------------
-- Game represented as two grids (2D arrays) - ApparentGrid + ActualGrid

-- ApparentGrid
-- - unopened = 9
-- - flagged = 10
-- - opened = 0-8 (num of mines adjacent)

-- ActualGrid
-- - -1 = mine
-- - 0-8 = num of surrounding mines

type ActualGrid = [[Int]]
type ApparentGrid = [[Int]]

type Coord = (Int, Int)

width :: [[a]] -> Int
width a = length $ a !! 0

height :: [[a]] -> Int
height a = length a

-- index into 2D array
-- accessed (row, col)
(@!!) :: Coord -> [[a]] -> a
(@!!) (x,y) g = g !! x !! y

-- Creating game grids
createActualGrid :: [Coord] -> Int -> Int -> ActualGrid
createActualGrid cs w h = [[isMine (i,j) cs | j <- [0..(w-1)]] | i <- [0..(h-1)]]

-- if coord in list of mine coords then mine (-1)
isMine :: Coord -> [Coord] -> Int
isMine c cs
    | c `elem` cs = -1
    | otherwise = 0

createApparentBoard :: Int -> Int -> ApparentGrid
createApparentBoard w h = replicate h (replicate w 9)

-- Number of mines adjacent to a coord
numAdjMines :: ActualGrid -> Coord -> Int
numAdjMines grid (i,j) =
    let conv = [(i1,j1) | i1 <- (oneAboveBelow i $ height grid),
                          j1 <- (oneAboveBelow j $ width grid)]
    in sumOfMines (deleteFirstFromList (i,j) conv) grid

sumOfMines :: [Coord] -> ActualGrid -> Int
sumOfMines [] grid = 0
sumOfMines (c:cs) grid = 
    case c @!! grid of
        -1 -> 1 + sumOfMines cs grid
        _ -> sumOfMines cs grid 

-- Unopened coords adjacent to a given coord
unopenedAdjCoords :: ApparentGrid -> Coord -> [Coord]
unopenedAdjCoords grid (i,j) =
    let conv = [(i1,j1) | i1 <- (oneAboveBelow i $ height grid),
                          j1 <- (oneAboveBelow j $ width grid)]
    in getUnopenedAdjCoords (deleteFirstFromList (i,j) conv) grid

getUnopenedAdjCoords :: [Coord] -> ApparentGrid -> [Coord]
getUnopenedAdjCoords [] _ = []
getUnopenedAdjCoords (c:cs) grid
    | c @!! grid == 9 = c:getUnopenedAdjCoords cs grid
    | otherwise = getUnopenedAdjCoords cs grid

-- Opened coords adjacent to a given coord
openedAdjCoords :: ApparentGrid -> Coord -> [Coord]
openedAdjCoords grid (i,j) = 
    let conv = [(i1,j1) | i1 <- (oneAboveBelow i $ height grid),
                          j1 <- (oneAboveBelow j $ width grid)]
    in getOpenedAdjCoords (deleteFirstFromList (i,j) conv) grid

getOpenedAdjCoords :: [Coord] -> ApparentGrid -> [Coord]
getOpenedAdjCoords [] _ = []
getOpenedAdjCoords (c:cs) grid
    | c @!! grid /= 9 && c @!! grid /= 10 = c:getOpenedAdjCoords cs grid
    | otherwise = getOpenedAdjCoords cs grid

getFlaggedCoords :: [Coord] -> ApparentGrid -> [Coord]
getFlaggedCoords [] _ = []
getFlaggedCoords (c:cs) grid
    | c @!! grid == 10 = c:getFlaggedCoords cs grid
    | otherwise = getFlaggedCoords cs grid
-- # flags adjacent to a given coord
numAdjFlags :: ApparentGrid -> Coord -> Int
numAdjFlags grid (i,j) = 
    let conv = [(i1,j1) | i1 <- (oneAboveBelow i $ height grid),
                          j1 <- (oneAboveBelow j $ width grid)]
    in sumOfFlags (deleteFirstFromList (i,j) conv) grid

sumOfFlags :: [Coord] -> ApparentGrid -> Int
sumOfFlags [] grid = 0
sumOfFlags (c:cs) grid = 
    case c @!! grid of
        10 -> 1 + sumOfFlags cs grid
        _ -> sumOfFlags cs grid

-- insert the hidden mine information
insertAdjMineInfo :: ActualGrid -> ActualGrid
insertAdjMineInfo grid =
    let h = height grid
        w = width grid 
    in [[ getNumAdjMines grid (i,j) | j <- [0..(w-1)]] | i <- [0..(h-1)]]
    
getNumAdjMines :: ActualGrid -> Coord -> Int
getNumAdjMines grid c
    | c @!! grid == -1 = -1
    | otherwise = numAdjMines grid c

-- Takes i and a bounding value and returns [i-1, i, i+1]
-- or as many of them as possible - eg. at edge of grid only -> [i, i+1]
-- Int -> maxbounds -> [Int]
oneAboveBelow :: Int -> Int -> [Int]
oneAboveBelow x max  
    | x == 0        = [x, x+1]
    | x == (max-1)  = [x-1,x]
    | otherwise     = [x-1,x,x+1]

-- Delete first matching element from list
deleteFirstFromList :: (Eq a) => a -> [a] -> [a]
deleteFirstFromList e (x:xs) 
    | e == x = xs
    | otherwise = x : deleteFirstFromList e xs 

-- Return new ApparentGrid with opened coord
openSquare :: ApparentGrid -> ActualGrid -> Coord -> ApparentGrid
openSquare apGrid acGrid c = 
    let h = height apGrid
        w = width apGrid 
    in [[ updateOpenSquare apGrid acGrid (i,j) c | j <- [0..(w-1)]] | i <- [0..(h-1)]]

-- Can't open a flagged square
updateOpenSquare :: ApparentGrid -> ActualGrid -> Coord -> Coord -> Int
updateOpenSquare apGrid acGrid c opened
    | c == opened && (c @!! apGrid == 10) = c @!! apGrid -- if flagged, stay flagged
    | c == opened = c @!! acGrid -- if not, show actual grid
    | otherwise = c @!! apGrid -- else don't touch

-- Return new grid with given coordinate flagged
flagSquare :: ApparentGrid -> Coord -> ApparentGrid
flagSquare apGrid c =
    let h = height apGrid
        w = width apGrid
    in [[ updateFlagSquare apGrid (i,j) c | j <- [0..(w-1)]]
                                          | i <- [0..(h-1)]]

-- Flag a list of coords
flagMultiple :: ApparentGrid -> [Coord] -> ApparentGrid
flagMultiple apGrid [] = apGrid
flagMultiple apGrid (c:cs) = 
    let newGrid = flagSquare apGrid c
    in flagMultiple newGrid cs

-- Endgame - if win then flag all unopened
flagAllUnmarked :: ApparentGrid -> ApparentGrid
flagAllUnmarked apGrid = 
    let h = height apGrid
        w = width apGrid
    in [[unmarkedToFlag apGrid (i,j) | j <- [0..(w-1)]] 
                                     | i <- [0..(h-1)]]

unmarkedToFlag :: ApparentGrid -> Coord -> Int
unmarkedToFlag grid c
    | c @!! grid == 9 = 10
    | otherwise = c @!! grid

-- Toggle flag
updateFlagSquare :: ApparentGrid -> Coord -> Coord -> Int
updateFlagSquare apGrid c flagged
    | (c @!! apGrid == 10) && c == flagged = 9
    | (c @!! apGrid == 9) && c == flagged = 10
    | otherwise = c @!! apGrid

-- Propagate openings when opening a square with 0 mines around
openZero :: ApparentGrid -> ActualGrid -> Coord -> ApparentGrid
openZero apGrid acGrid coord = 
    let is = oneAboveBelow (fst coord) (height acGrid)
        js = oneAboveBelow (snd coord) (width acGrid)
        open = [(i,j) | i <- is,
                        j <- js]
        coords = spacesToOpen acGrid open []
    in openMultipleSquares apGrid acGrid coords

spacesToOpen :: ActualGrid -> [Coord] -> [Coord] -> [Coord]
spacesToOpen _ [] acc = acc
spacesToOpen acGrid (c:cs) acc = 
    if c @!! acGrid == 0 then
        if c `notElem` acc then do
            let extList = nub $ cs ++ (getSurroundCoords acGrid c) 
            spacesToOpen acGrid extList (c:acc)
        else spacesToOpen acGrid cs acc
    else spacesToOpen acGrid cs (c:acc)

-- Get coordinates of all mines
getMineCoords :: ActualGrid -> [Coord]
getMineCoords acGrid = 
    let h = height acGrid
        w = width acGrid
        allCoords = [(i,j) | i <- [0..(h-1)],
                             j <- [0..(w-1)]]
    in getMines acGrid allCoords

getMines :: ActualGrid -> [Coord] -> [Coord]
getMines acGrid (c:cs) = filter (\n -> n @!! acGrid == -1) (c:cs) 

-- 'F' = flag else uncover square
handleInput :: ApparentGrid -> ActualGrid -> Coord -> String -> ApparentGrid
handleInput apGrid acGrid coord instr = do
    case instr of
        "F" -> flagSquare apGrid coord
        _ -> do
            if coord @!! acGrid == (-1) then
                openSquare apGrid acGrid coord
            else 
                if (coord @!! acGrid == 0 || coord @!! apGrid /= 9) && (numAdjFlags apGrid coord) == (numAdjMines acGrid coord) then
                    openZero apGrid acGrid coord
                else
                    openSquare apGrid acGrid coord

-- Check for each endgame scenario
checkEndgame :: ApparentGrid -> ActualGrid -> String
checkEndgame apGrid acGrid
    | checkMineTrip apGrid = "L"
    | checkWin apGrid acGrid = "W"
    | otherwise = "P"

checkMineTrip :: ApparentGrid -> Bool
checkMineTrip grid = (-1) `elem` (concat grid)

checkWin :: ApparentGrid -> ActualGrid -> Bool
checkWin apGrid acGrid = 
    let cat = concat apGrid
        numMines = getNumMines acGrid
        numHidden = getNumHiddenSquares apGrid
    in numMines == numHidden

getNumHiddenSquares :: ApparentGrid -> Int
getNumHiddenSquares grid = 
    let cat = concat grid
    in length $ filter (\x -> x==9 || x==10) cat

getSurroundCoords :: ActualGrid -> Coord -> [Coord]
getSurroundCoords grid c = [(i,j) | i <- (oneAboveBelow (fst c) (height grid)),
                                    j <- (oneAboveBelow (snd c) (width grid))]

checkZero :: ApparentGrid -> ActualGrid -> [Coord] -> ApparentGrid
checkZero apGrid _ [] = apGrid
checkZero apGrid acGrid (c:cs) =
    if c @!! acGrid == 0 then
        let is = oneAboveBelow (fst c) (height acGrid)
            js = oneAboveBelow (snd c) (width acGrid)
            open = [(i,j) | i <- is,
                            j <- js]
            toOpen = deleteFirstFromList c open
            newGrid = openMultipleSquares apGrid acGrid toOpen
            ng = checkZero newGrid acGrid toOpen
        in 
            checkZero ng acGrid cs
    else
        let newGrid = openSquare apGrid acGrid c 
        in checkZero apGrid acGrid cs

-- Open a list of coordinates
openMultipleSquares :: ApparentGrid -> ActualGrid -> [Coord] -> ApparentGrid
openMultipleSquares apGrid acGrid [] = apGrid
openMultipleSquares apGrid acGrid (c:cs) = 
    let newGrid = openSquare apGrid acGrid c
    in openMultipleSquares newGrid acGrid cs

getNumMines :: ActualGrid -> Int
getNumMines grid =
    let cat = concat grid
    in length $ filter (==(-1)) cat

getNumFlags :: ApparentGrid -> Int
getNumFlags grid = 
    let cat = concat grid
    in length $ filter (==10) cat

getNumUnopened :: ApparentGrid -> Int
getNumUnopened grid = 
    let cat = concat grid
    in length $ filter (==9) cat

----------------------------
-- Random Mine Generation --
----------------------------

-- Generate random mine until we get one not in the given list
randomCoordNotInList :: [Coord] -> Int -> Int -> StdGen -> (Coord, StdGen)
randomCoordNotInList cs w h g = 
    let (c,g') = randomCoord w h g
    in 
        if c `elem` cs then randomCoordNotInList cs w h g'
        else (c, g')

-- Bounded random number generation 
randomCoord :: Int -> Int -> StdGen -> (Coord, StdGen)
randomCoord w h g =
    let (r1, g')  = randomR (0, h-1) g
        (r2, g'') = randomR (0, w-1) g'
    in ((r1, r2), g'') 

randomCoords :: [Coord] -> Int -> Int -> Int-> StdGen -> ([Coord], StdGen)
randomCoords _ _ _ 0 g = ([], g)
randomCoords cs w h num g =
    if w * h <= num then
        error("Too many coordinates requested")
    else
        let (c, g') = randomCoordNotInList cs w h g
            (css, g'') = randomCoords (c:cs) w h (num-1) g'
        in (c:css, g'')

-- Initialise game returning ApparentGrid and ActualGrid
initGame :: Int -> Int -> Int -> Int -> (ApparentGrid, ActualGrid, StdGen)
initGame g numMines w h = do
    let (mineCoords,g1) = randomCoords [] w h numMines (mkStdGen g)
    let actualGrid = insertAdjMineInfo $ createActualGrid mineCoords w h
    let appGrid = createApparentBoard w h
    (appGrid, actualGrid, g1)


-- main :: IO ()
-- main = do
--     time <- timeInt  
--     let (mineCoords,g) = randomCoords [] 9 9 2 (mkStdGen time)

--     let actualGrid = insertAdjMineInfo $ createActualGrid mineCoords 9 9
--     let appGrid = createApparentBoard 9 9

--     userInput appGrid actualGrid
    