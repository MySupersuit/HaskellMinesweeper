module AIPlayer 
    ( markObviousMines
    , aiMove
    , markNonObvious
    , rref
    , aiNonObviousMove
    , probMove)
where

import Minesweeper
import qualified Data.List as L

aiMove :: ApparentGrid -> ActualGrid -> Int -> ApparentGrid
aiMove apGrid acGrid numMines = do
    let marked = markObviousMines apGrid
    let newGrid = openObviousSpaces marked acGrid numMines
    newGrid
    -- if newGrid == apGrid then do
    --     markNonObvious apGrid acGrid
    --     else do
    --         newGrid

-- aiNonObviousMove :: ApparentGrid -> ActualGrid -> (ApparentGrid, [[Float]])
-- aiNonObviousMove :: ApparentGrid -> ActualGrid -> ([Coord], [Coord]) 

aiNonObviousMove apGrid acGrid = do
    let marked = markNonObvious apGrid acGrid
    marked
    -- (apGrid, marked)

-- for each square, if opened -> if #unopened adj == num mines adjacent then flag all
-- shoudl return coord + instruction rather than new board?

markObviousMines :: ApparentGrid -> ApparentGrid
markObviousMines grid =
    let h = height grid
        w = width grid
        allCoords = [(i,j) | i <- [0..(w-1)],
                             j <- [0..(h-1)]]
    in markObvious grid allCoords 

markObvious :: ApparentGrid -> [Coord] -> ApparentGrid
markObvious grid [] = grid 
markObvious grid (c:cs)
    | c @!! grid == 9 = markObvious grid cs 
    | c @!! grid == 10 = markObvious grid cs 
    | c @!! grid == 0 = markObvious grid cs 
    | otherwise = do
        let unopened = getNumUnopenedAdj grid c
        if unopened == 0 then do markObvious grid cs
            else do
                let numMines = c @!! grid
                let numFlags = numAdjFlags grid c
                -- if numMines == unopened && numMines > numFlags then
                if numFlags < c @!! grid && numMines-numFlags == unopened then
                    flagMultiple grid (unopenedAdjCoords grid c)
                    else
                        markObvious grid cs

openObviousSpaces :: ApparentGrid -> ActualGrid -> Int -> ApparentGrid
openObviousSpaces apGrid acGrid numMines=
    let h = height apGrid
        w = width apGrid
        allCoords = [(i,j) | i <- [0..(w-1)],
                             j <- [0..(h-1)]]
    in openObvious apGrid acGrid allCoords numMines

openObvious :: ApparentGrid -> ActualGrid -> [Coord] -> Int -> ApparentGrid
openObvious apGrid acGrid [] _ = apGrid
openObvious apGrid acGrid (c:cs) 0
    | c @!! apGrid == 9 = do
        let newGrid = openMultipleSquares apGrid acGrid [c]
        openObvious newGrid acGrid cs 0
    | otherwise = openObvious apGrid acGrid cs 0
openObvious apGrid acGrid (c:cs) numMines
    | c @!! apGrid == 9 = openObvious apGrid acGrid cs numMines
    | c @!! apGrid == 10 = openObvious apGrid acGrid cs numMines
    | c @!! apGrid == 0 = openObvious apGrid acGrid cs numMines
    | otherwise = do
        let unopened = getNumUnopenedAdj apGrid c
        if unopened == 0 then do openObvious apGrid acGrid cs numMines
            else do
                let numFlags = numAdjFlags apGrid c
                if numFlags == c @!! apGrid then
                    openZero apGrid acGrid c
                    else
                        openObvious apGrid acGrid cs numMines


-- markNonObvious :: ApparentGrid -> ActualGrid -> (ApparentGrid, [[Float]])
-- markNonObvious :: ApparentGrid -> ActualGrid -> [[Float]]
-- markNonObvious :: ApparentGrid -> ActualGrid -> ([Coord], [Coord])
markNonObvious :: ApparentGrid -> ActualGrid -> (ApparentGrid,[[Float]], [Coord], [Coord])
markNonObvious apGrid acGrid = do
    let h = height apGrid
    let w = width apGrid
    let allCoords = [(i,j) | i <- [0..(w-1)],
                             j <- [0..(h-1)]]
    
    let borderUn = L.nub $ getBorderUnopened apGrid allCoords
    let borderOp = L.nub $ getBorderOpened apGrid allCoords
    -- (borderOp, borderUn)
    let ref = rref $ makeMatrix borderOp borderUn apGrid

    let (mines, opens) = new_rrefToMove ref
    -- (ref, mines, opens)
    let toFlag = isToCoords mines borderUn
    let toOpen = isToCoords opens borderUn

    let midGrid1 = flagMultiple apGrid toFlag
    let midGrid2 = openMultipleSquares midGrid1 acGrid toOpen

    let newGrid = checkZeros toOpen midGrid2 acGrid


    -- newGrid
    (newGrid, ref, toFlag, toOpen)
    -- (toFlag, toOpen)


checkZeros :: [Coord] -> ApparentGrid -> ActualGrid -> ApparentGrid
checkZeros [] apGrid _ = apGrid
checkZeros (c:cs) apGrid acGrid
    | c @!! apGrid == 0 = do
        let newGrid = openZero apGrid acGrid c
        checkZeros cs newGrid acGrid
    | otherwise = checkZeros cs apGrid acGrid


isToCoords :: [Int] -> [Coord] -> [Coord]
isToCoords [] _ = []
isToCoords (i:is) cs = (cs !! i) : isToCoords is cs 


-- takes init of list
getMaxMinBounds :: [Float] -> (Int, Int)
getMaxMinBounds ref =
    let l1 = length (filter (==1.0) ref)
        l2 = (-1) * (length (filter (==(-1.0)) ref))
    in (l1, l2)

-- new_rrefToMove :: [[Float]] -> [([Int], Float)]
new_rrefToMove :: [[Float]] -> ([Int], [Int])
-- new_rrefToMove :: [[Float]] -> (Int, Int)
new_rrefToMove [] = ([],[])
-- new_rrefToMove [] = (-1,-1)
new_rrefToMove (r:ef) = do
    let row = init r
    let (max, min) = getMaxMinBounds $ row
    -- (max,min)
    let val = round $ last r
    -- (val, min)
    if max == val && max /= 0 then do
        let mines = indicesOf 1.0 row
        let open = indicesOf (-1.0) row
        (mines,open)
        else do
            if min == val then do
                let mines = indicesOf (-1.0) row
                let open = indicesOf 1.0 row
                (mines,open)
                else do
                    new_rrefToMove ef

answerFound :: [Float] -> Bool
answerFound row = 
    let start = init row
        len = length $ init start
    in ((length $ filter (==0) start) == len) && ((length $ filter (==1) start) == 1)

indicesOf :: Float -> [Float] -> [Int]
indicesOf elem list = indicesOfAcc elem list 0

indicesOfAcc :: Float -> [Float] -> Int -> [Int]
indicesOfAcc _ [] _ = []
indicesOfAcc elem (x:xs) i
    | elem == x = i : indicesOfAcc elem xs (i+1)
    | otherwise = indicesOfAcc elem xs (i+1) 

-- indexOf :: Float -> [Float] -> Int
-- indexOf elem list = indexOfAcc elem list 0

-- indexOfAcc :: Float -> [Float] -> Int -> Int
-- indexOfAcc _ [] _ = -1
-- indexOfAcc elem (x:xs) acc
--     | elem == x = acc
--     | otherwise = indexOfAcc elem xs (acc+1)


getBorderOpened :: ApparentGrid -> [Coord] -> [Coord]
getBorderOpened grid [] = []
getBorderOpened grid (c:cs)
    | unopenedAdjCoords grid c == [] = getBorderOpened grid cs
    | c @!! grid > 0 && c @!! grid < 9 && unopenedAdjCoords grid c /= [] = c : getBorderOpened grid cs
    | otherwise = getBorderOpened grid cs

getBorderUnopened :: ApparentGrid -> [Coord] -> [Coord]
getBorderUnopened grid [] = []
getBorderUnopened grid (c:cs)
    | openedAdjCoords grid c == [] = getBorderUnopened grid cs
    | c @!! grid == 9 && openedAdjCoords grid c /= [] = c : getBorderUnopened grid cs
    | otherwise = getBorderUnopened grid cs
    
-- getBorderOpened :: ApparentGrid -> [Coord] -> [Coord]
-- getBorderOpened grid [] = []
-- getBorderOpened grid (c:cs)
--     | un

-- solve system of equations using cramers rule or something?
-- look at matrix package

-- opened
listBs :: [Coord] -> ApparentGrid -> [Int]
listBs [] grid = []
listBs (o:op) grid = y1 : listBs op grid
    where y1 = o @!! grid - numAdjFlags grid o 

toOneZeroRowVect :: [Coord] -> [Coord] -> [Float]
toOneZeroRowVect [] cs = []
toOneZeroRowVect (u:un) cs 
    | u `elem` cs = 1.0 : toOneZeroRowVect un cs
    | otherwise = 0.0 : toOneZeroRowVect un cs

makeMatrix :: [Coord] -> [Coord] -> ApparentGrid -> [[Float]]
makeMatrix [] _ _ = []
makeMatrix (o:op) unopened grid = do
    -- let bs = listBs (o:op) grid
    -- bs
    let b1 = fromIntegral $ o @!! grid - numAdjFlags grid o
    let u = unopenedAdjCoords grid o
    let rhs = toOneZeroRowVect unopened u
    (rhs ++ [b1]) : makeMatrix op unopened grid
    -- rhs : makeMatrix op unopened grid


-- convert matrix into reduced row echelon form
-- from https://rosettacode.org/wiki/Reduced_row_echelon_form#Haskell 
rref :: (Eq a, Fractional a) => [[a]] -> [[a]]
rref m = f m 0 [0 .. rows - 1]
  where rows = length m
        cols = length $ head m
 
        f m _    []              = m
        f m lead (r : rs)
            | indices == Nothing = m
            | otherwise          = f m' (lead' + 1) rs
          where indices = L.find p l
                p (col, row) = m !! row !! col /= 0
                l = [(col, row) |
                    col <- [lead .. cols - 1],
                    row <- [r .. rows - 1]]
 
                Just (lead', i) = indices
                newRow = map (/ m !! i !! lead') $ m !! i
 
                m' = zipWith g [0..] $
                    replace r newRow $
                    replace i (m !! r) m
                g n row
                    | n == r    = row
                    | otherwise = zipWith h newRow row
                  where h = subtract . (* row !! lead')

replace :: Int -> a -> [a] -> [a]
{- Replaces the element at the given index. -}
replace n e l = a ++ e : b
  where (a, _ : b) = splitAt n l

freshGrid [] = True
freshGrid (c:cs) =
    case c of 
        9 -> freshGrid cs
        _ -> False 

probMove apGrid acGrid numMines =
    case freshGrid (concat apGrid) of
        True -> firstMove apGrid acGrid
        False -> naiveProb apGrid acGrid numMines

firstMove apGrid acGrid = do
    let h = height apGrid
    let w = width apGrid
    let midCoord = (h `div` 2, w `div` 2)
    let openGrid = openMultipleSquares apGrid acGrid [midCoord]
    let newGrid = checkZeros [midCoord] openGrid acGrid
    newGrid

-- naiveProb :: ApparentGrid -> ActualGrid -> [Coord]
naiveProb apGrid acGrid numMines = do
    let h = height apGrid
    let w = width apGrid
    let allCoords = [(i,j) | i <- [0..(w-1)],
                             j <- [0..(h-1)]]
    
    let borderUn = L.nub $ getBorderUnopened apGrid allCoords
    let borderOp = L.nub $ getBorderOpened apGrid allCoords

    let probs = getProbs apGrid borderUn
    let minProb = minimum probs
    let indexToOpen = indicesOf minProb probs
    let coordToOpen = borderUn !! (head indexToOpen)
 
    let boardProb = getBoardProb apGrid numMines
    if boardProb < minProb then do
        let opened = getOpenedAdjCoords allCoords apGrid
        let otherCoordToOpen = getCoordNotIn allCoords (borderUn++opened)
        let openGrid = openMultipleSquares apGrid acGrid [otherCoordToOpen]
        let newGrid = checkZeros [otherCoordToOpen] openGrid acGrid
        newGrid
        else do
            let openGrid = openMultipleSquares apGrid acGrid [coordToOpen]
            let newGrid = checkZeros [coordToOpen] openGrid acGrid
            -- (newGrid, probs, coordToOpen, boardProb)
            newGrid


getCoordNotIn all coords = do
    let diff = all L.\\ coords
    diff !! ((length diff) `div` 2)  

-- getProbs :: ApparentGrid -> [Coord] -> [Float]
getProbs apGrid [] = []
getProbs apGrid (c:cs) = do
    let op = openedAdjCoords apGrid c
    let prob = calcProb apGrid op
    prob : getProbs apGrid cs


mean :: [Float] -> Float
mean lst = (sum lst) / (fromIntegral $ length lst)

calcProb :: ApparentGrid -> [Coord] -> Float
calcProb _ [] = 0
calcProb apGrid (c:cs) = do
    let adj = fromIntegral $ length $ unopenedAdjCoords apGrid c
    let f = fromIntegral$ numAdjFlags apGrid c
    let m = fromIntegral $ c @!! apGrid
    ((m - f) / adj) + (calcProb apGrid cs)

getBoardProb apGrid numMines = (fromIntegral numMines) / fromIntegral (getNumUnopened apGrid)
