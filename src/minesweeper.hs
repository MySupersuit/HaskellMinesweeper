module Minesweeper 
    ( ApparentGrid
    , Coord
    )
where

import System.Random
import Data.Time.Clock.POSIX
import Data.List
import Control.Monad

-- size of grid
-- number of mines
-- actual grid values (mine locations)
-- apparent grid values
-- flagged positions

data Space = Open | Close | Flag

type ActualGrid = [[Int]]
type ApparentGrid = [[Int]]
-- 0 = unopened
-- 1 = opened
-- 2 = flagged?

type Coord = (Int, Int)

width :: [[a]] -> Int
width a = length $ a !! 0

height :: [[a]] -> Int
height a = length a

createActualBoard :: [Coord] -> Int -> Int -> ActualGrid
createActualBoard cs h w = [[isMine (i,j) cs | j <- [0..(w-1)]] | i <- [0..(h-1)]]

createApparentBoard :: Int -> Int -> ApparentGrid
createApparentBoard h w = replicate h (replicate w 9)
-- 9 = unopened space

-- -1 = Mine
-- 0 = Empty
-- Other numbers will mean number of mines adjacent
isMine :: Coord -> [Coord] -> Int
isMine c cs
    | c `elem` cs = -1
    | otherwise = 0

-- numAdjMines :: ActualGrid -> Coord -> [Coord]
numAdjMines :: ActualGrid -> Coord -> Int
numAdjMines grid (i,j) =
    let conv = [(i1,j1) | i1 <- (oneAboveBelow i $ height grid),
                          j1 <- (oneAboveBelow j $ width grid)]
    in sumOfMines (deleteFirstFromList (i,j) conv) grid

getNumAdjMines :: ActualGrid -> Coord -> Int
getNumAdjMines grid c
    | c @!! grid == -1 = -1
    | otherwise = numAdjMines grid c


insertAdjMines :: ActualGrid -> ActualGrid
insertAdjMines grid =
    let h = height grid
        w = width grid 
    in [[ getNumAdjMines grid (i,j) | j <- [0..(w-1)]] | i <- [0..(h-1)]]
    

-- takes i and a bounding value and returns [i-1, i, i+1]
-- or as many of them as possible
-- Int -> maxbounds -> [Int]
oneAboveBelow :: Int -> Int -> [Int]
oneAboveBelow x max  
    | x == 0        = [x, x+1]
    | x == (max-1)  = [x-1,x]
    | otherwise     = [x-1,x,x+1]

-- specify what you're looking for in surrounding squares
-- sumOfSurrounds :: [Coord] -> ActualGrid -> Int -> Int
-- sumOfSurrounds [] _ _ = 0
-- sumOfSurrounds (c:cs) grid x =
--     case c @!! grid of
--         x -> 1 + sumOfSurrounds cs grid x
--         _ -> sumOfSurrounds cs grid x

sumOfMines :: [Coord] -> ActualGrid -> Int
sumOfMines [] grid = 0
sumOfMines (c:cs) grid = 
    case c @!! grid of
        -1 -> 1 + sumOfMines cs grid
        _ -> sumOfMines cs grid 


deleteFirstFromList :: (Eq a) => a -> [a] -> [a]
deleteFirstFromList e (x:xs) 
    | e == x = xs
    | otherwise = x : deleteFirstFromList e xs

-- index into 2D array
-- accessed (row, col)
(@!!) :: Coord -> [[a]] -> a
(@!!) (x,y) g = g !! x !! y

randomCoordNotInList :: [Coord] -> Int -> Int -> StdGen -> (Coord, StdGen)
randomCoordNotInList cs w h g = 
    let (c,g') = randomCoord w h g
    in 
        if c `elem` cs then randomCoordNotInList cs w h g'
        else (c, g')

randomCoord :: Int -> Int -> StdGen -> (Coord, StdGen)
randomCoord w h g =
    let (r1, g')  = randomR (0, w-1) g
        (r2, g'') = randomR (0, h-1) g'
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


-- for generating random starting positions each time
timeInt :: IO Int
timeInt = round . (1000 *) <$> getPOSIXTime

prettyPrint [] = return ()
prettyPrint (g:gs) = do
    print g
    prettyPrint gs

apparentPrint grid = prettyPrint $ transformApparent grid

transformApparent grid = 
    let h = height grid
        w = width grid 
    in [[apparentTransform grid (i,j) | j <- [0..(w-1)]] | i <- [0..(h-1)]] 

apparentTransform :: ApparentGrid -> Coord -> String
apparentTransform grid c = 
    case c @!! grid of
        0       -> "_"
        9       -> "#"
        10      -> "F"
        _       -> show $ c @!! grid     


openSquare :: ApparentGrid -> ActualGrid -> Coord -> ApparentGrid
openSquare apGrid acGrid c = 
    let h = height apGrid
        w = width apGrid 
    in [[ updateOpenSquare apGrid acGrid (i,j) c | j <- [0..(w-1)]] | i <- [0..(h-1)]]

updateOpenSquare :: ApparentGrid -> ActualGrid -> Coord -> Coord -> Int
updateOpenSquare apGrid acGrid c opened
    | c == opened = c @!! acGrid
    | otherwise = c @!! apGrid

flagSquare :: ApparentGrid -> Coord -> ApparentGrid
flagSquare apGrid c =
    let h = height apGrid
        w = width apGrid
    in [[ updateFlagSquare apGrid (i,j) c | j <- [0..(w-1)]]
                                          | i<- [0..(h-1)]]

updateFlagSquare :: ApparentGrid -> Coord -> Coord -> Int
updateFlagSquare apGrid c flagged
    | c == flagged = 10
    | otherwise = c @!! apGrid

openZero :: ApparentGrid -> ActualGrid -> Coord -> ApparentGrid
openZero apGrid acGrid coord = 
    let is = oneAboveBelow (fst coord) (height acGrid)
        js = oneAboveBelow (snd coord) (width acGrid)
        open = [(i,j) | i <- is,
                        j <- js]

        coords = spacesToOpen acGrid open []
        -- newGrid = openMultipleSquares apGrid acGrid coords
    in openMultipleSquares apGrid acGrid coords
    -- in do
    --     userInput newGrid acGrid

handleInput :: ApparentGrid -> ActualGrid -> String -> ApparentGrid
handleInput apGrid acGrid input = do
    let splits = words input

    case splits !! 0 of
        "F" -> do
            let coord = listStringToCoord $ tail splits
            flagSquare apGrid coord
        _ -> do   -- if number
            let coord = listStringToCoord $ splits

            if coord @!! acGrid == 0 then
                openZero apGrid acGrid coord
            else
                openSquare apGrid acGrid coord

checkEndgame :: ApparentGrid -> ActualGrid -> Bool
checkEndgame apGrid acGrid
    | checkMineTrip apGrid = True
    | checkWin apGrid acGrid = True
    | otherwise = False
    -- let h = height apGrid
    --     w = width apGrid
    -- in [[ checkEndSquare apGrid (i,j) c | j <- [0..(w-1)]]
    --                                     | i<- [0..(h-1)]]

checkMineTrip :: ApparentGrid -> Bool
checkMineTrip grid = (-1) `elem` (concat grid)

checkWin :: ApparentGrid -> ActualGrid -> Bool
checkWin apGrid acGrid = 
    let cat = concat apGrid
        numMines = getNumMines acGrid
        numHidden = getNumHiddenSquares apGrid
    in numMines == numHidden
-- checkEndSquare :: ApparentGrid -> ActualGrid -> Coord -> Int

getNumHiddenSquares :: ApparentGrid -> Int
getNumHiddenSquares grid = 
    let cat = concat grid
    in length $ filter (\x -> x==9 || x==10) cat

userInput :: ActualGrid -> ActualGrid -> IO ()
userInput apGrid acGrid = do
    putStrLn ""
    prettyPrint acGrid
    putStrLn ""
    prettyPrint apGrid
    apparentPrint apGrid

    if checkEndgame apGrid acGrid then do
        putStrLn "GAME OVER!"
    else do
        line <- getLine
        let newGrid = handleInput apGrid acGrid line

        userInput newGrid acGrid

getSurroundCoords :: ActualGrid -> Coord -> [Coord]
getSurroundCoords grid c = [(i,j) | i <- (oneAboveBelow (fst c) (height grid)),
                                    j <- (oneAboveBelow (snd c) (width grid))]

spacesToOpen :: ActualGrid -> [Coord] -> [Coord] -> [Coord]
spacesToOpen _ [] acc = acc
spacesToOpen acGrid (c:cs) acc = 
    if c @!! acGrid == 0 then
        if c `notElem` acc then do
            let extList = nub $ cs ++ (getSurroundCoords acGrid c) 
            spacesToOpen acGrid extList (c:acc)
        else spacesToOpen acGrid cs acc
    else spacesToOpen acGrid cs (c:acc)

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

openMultipleSquares :: ApparentGrid -> ActualGrid -> [Coord] -> ApparentGrid
openMultipleSquares apGrid acGrid [] = apGrid
openMultipleSquares apGrid acGrid (c:cs) = 
    let newGrid = openSquare apGrid acGrid c
    in openMultipleSquares newGrid acGrid cs


listStringToCoord :: [String] -> Coord
listStringToCoord (c:s:_) = (read c,read s)

getNumMines :: ActualGrid -> Int
getNumMines grid =
    let cat = concat grid
    in length $ filter (==(-1)) cat

-- from webpage -> send coordinates which are input coordinates
-- left click = uncover
-- right click = flag
-- button to reset

main :: IO ()
main = do
    time <- timeInt  
    let (mineCoords,g) = randomCoords [] 9 9 2 (mkStdGen time)

    let actualGrid = insertAdjMines $ createActualBoard mineCoords 9 9
    let appGrid = createApparentBoard 9 9

    userInput appGrid actualGrid
    