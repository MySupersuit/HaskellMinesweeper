module AIPlayer 
    ( markObviousMines
    , aiMove
    , markNonObvious
    , rref
    , aiNonObviousMove)
where

-- import Prelude hiding (map, foldl, zipWith)
import Minesweeper
import qualified Data.List as L
-- import Data.Matrix
-- import Math.LinearEquationSolver

aiMove :: ApparentGrid -> ActualGrid -> ApparentGrid
aiMove apGrid acGrid = do
    let marked = markObviousMines apGrid
    let newGrid = openObviousSpaces marked acGrid
    newGrid
    -- if newGrid == apGrid then do
    --     markNonObvious apGrid acGrid
    --     else do
    --         newGrid

aiNonObviousMove :: ApparentGrid -> ActualGrid -> (ApparentGrid, [[Float]])
aiNonObviousMove apGrid acGrid = markNonObvious apGrid acGrid

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

openObviousSpaces :: ApparentGrid -> ActualGrid -> ApparentGrid
openObviousSpaces apGrid acGrid =
    let h = height apGrid
        w = width apGrid
        allCoords = [(i,j) | i <- [0..(w-1)],
                             j <- [0..(h-1)]]
    in openObvious apGrid acGrid allCoords 

openObvious :: ApparentGrid -> ActualGrid -> [Coord] -> ApparentGrid
openObvious apGrid acGrid [] = apGrid
openObvious apGrid acGrid (c:cs)
    | c @!! apGrid == 9 = openObvious apGrid acGrid cs
    | c @!! apGrid == 10 = openObvious apGrid acGrid cs
    | c @!! apGrid == 0 = openObvious apGrid acGrid cs
    | otherwise = do
        let unopened = getNumUnopenedAdj apGrid c
        if unopened == 0 then do openObvious apGrid acGrid cs
            else do
                let numFlags = numAdjFlags apGrid c
                if numFlags == c @!! apGrid then
                    openZero apGrid acGrid c
                    else
                        openObvious apGrid acGrid cs


markNonObvious :: ApparentGrid -> ActualGrid -> (ApparentGrid, [[Float]])
markNonObvious apGrid acGrid = do
    let h = height apGrid
    let w = width apGrid
    let allCoords = [(i,j) | i <- [0..(w-1)],
                             j <- [0..(h-1)]]
    
    let borderUn = L.nub $ getBorderUnopened apGrid allCoords
    let borderOp = L.nub $ getBorderOpened apGrid allCoords
    -- (borderOp, borderUn)
    let ref = rref $ makeMatrix borderOp borderUn apGrid
    let (index, move) = rrefToMove ref
    if index == -1 then do 
        (apGrid, [[-1.0]])
        else do
            let c = borderUn !! index
    -- (c,move)
            if move == 1.0 then do (flagMultiple apGrid [c], ref)
                else do 
                    let numFlags = numAdjFlags apGrid c
                    if numFlags == c @!! apGrid then
                        (openZero apGrid acGrid c, ref)
                        else do
                            (openMultipleSquares apGrid acGrid [c], ref)


    -- if move == 1.0 then do  
    -- if sum (init list) = 'last' element then we know the mines

    -- for each row in list --> get init of list
    -- if there's 1 1.0 there then the 'last' element tells whether 
    -- there's a mine or not there --> flag or openZero it then done.
    -- fromList (length borderUn, length borderOp) (concat $ makeMatrix borderOp borderUn grid)

-- rrefToMove :: 
rrefToMove [] = (-1,0)
rrefToMove (row:rows) 
    | answerFound row = (indexOf 1 row, last row)
    | otherwise = rrefToMove rows

answerFound row = 
    let start = init row
        len = length $ init start
    in ((length $ filter (==0) start) == len) && ((length $ filter (==1) start) == 1)

indexOf elem list = indexOfAcc elem list 0

indexOfAcc _ [] _ = -1
indexOfAcc elem (x:xs) acc
    | elem == x = acc
    | otherwise = indexOfAcc elem xs (acc+1)


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


-- cramers :: Matrix a -> Matrix a -> Matrix a
-- cramers a b = do
--     let det = detLU a

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

-- gaussianReduce :: [[Float]] -> [[Float]]
-- gaussianReduce matrix = fixlastrow $ foldl reduceRow matrix [0..length matrix-1] 
--     where
 
--  --swaps element at position a with element at position b.
--     swap xs a b
--         | a > b = swap xs b a
--         | a == b = xs
--         | a < b = let
--         (p1,p2) = splitAt a xs
--         (p3,p4) = splitAt (b-a-1) (tail p2)
--         in p1 ++ [xs!!b] ++ p3 ++ [xs!!a] ++ (tail p4)
 
--     reduceRow matrix1 r = 
--         let
--   --first non-zero element on or below (r,r).
--             firstnonzero = head $ filter (\x -> matrix1 !! x !! r /= 0) [r..length matrix1-1]
 
--   --matrix with row swapped (if needed)
--             matrix2 = swap matrix1 r firstnonzero
 
--   --row we're working with
--             row = matrix2 !! r
 
--   --make it have 1 as the leading coefficient
--             row1 = map (\x -> x / (row !! r)) row
 
--   --subtract nr from row1 while multiplying
--             subrow nr = let k = nr!!r in zipWith (\a b -> k*a - b) row1 nr
 
--   --apply subrow to all rows below
--             nextrows = map subrow $ drop (r+1) matrix2
 
--   --concat the lists and repeat
--         in take r matrix2 ++ [row1] ++ nextrows
 
--     fixlastrow matrix' = 
--         let
--             a = init matrix'; row = last matrix'; z = last row; nz = last (init row)
--         in a ++ [init (init row) ++ [1, z / nz]]

