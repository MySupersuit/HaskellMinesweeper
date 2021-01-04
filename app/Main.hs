module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Time.Clock.POSIX
import Reactive.Threepenny
import System.Random
import Minesweeper
import AIPlayer
import Data.Time
-- import System.Locale
import Control.Applicative
import Data.Matrix
-- import Math.LinearEquationSolver

import Data.IORef
import Control.Monad.Trans (liftIO)

-- TODO

-- reveal mines on loss

-- next level of non obvious moves - multisquare matrices
-- if grid == grid after obvious moves then try non obvious
-- firtly have to find area to analyse - borders of opened squares?

-- Select difficulty levels
--      #mines, board size 

-- AI - num unopened = num mines around ==> both mines
-- num marked mines = num mines then open the rest
-- Tank solver alg

-- setup
canvasHeight :: Int
canvasHeight = 600

canvasWidth :: Int
canvasWidth = 600

main :: IO ()
main = do
    -- problem with random nums
    -- only setup reruns on reload so same initial generator stays
    -- need to call start new game explicitly to get a new board
    -- init game in here giving an apparent board
    -- num <- randomIO :: IO Int
    -- new sequence of boards every time prog is LOADED (As it's in main)
    epoch_int <- (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int
    -- g <- mkStdGen num
    let boardstate = initGame epoch_int
    startGUI defaultConfig (setup boardstate)

setup :: (ApparentGrid, ActualGrid, StdGen) -> Window -> UI ()
setup (apparentGrid, actualGrid, g1) window = do
    return window # set title "Minesweeper"

    apGridRef <- liftIO $ newIORef apparentGrid
    acGridRef <- liftIO $ newIORef actualGrid
    genRef <- liftIO $ newIORef g1
    mousePos <- liftIO $ newIORef (0,0)
    gameStatusRef <- liftIO $ newIORef "P"
    -- mineCountRef <- liftIO $ newIORef 9 -- num mines


    -- TODO: set width and height based on size of grid
    display <- UI.span # set text "empty" -- testing purposes
    d2 <- UI.span # set text "empty"
    mineCountDisp <- UI.span # set text ("Mines: " ++ show (getNumMines actualGrid))
    canvas <- UI.canvas 
        # set UI.width 600 
        # set UI.height 600 
        # set UI.style [("border", "solid black 1px"), ("background", "#000")]
        # set UI.id_ "canvas"
    -- openMode <- UI.button #+ [string "Open"]
    -- markMode <- UI.button #+ [string "Mark"]
    newGame <- UI.button #+ [string "New Board"]
    makeMove <- UI.button #+ [string "Make Move"]
    makeNonObvs <- UI.button #+ [string "Make Non-Obvious Move"]

    -- drawGame (concat (replicate 10 (replicate 10 0))) 10 10 canvas 0
    ag <- liftIO $ readIORef apGridRef
    drawGame (concat ag) 9 9 canvas 0 "P" gameStatusRef

    getBody window #+
        [ column [element canvas]
        , element newGame, element makeMove, element makeNonObvs]

    getBody window #+ [row [element mineCountDisp],
                        row [element display],
                        row [element d2]]

    runFunction setNoContextMenu -- turns off context menu on right click

    on UI.click newGame $ \_ -> do
        gen <- liftIO $ readIORef genRef
        let (num, gen1) = random gen
        
        liftIO $ writeIORef genRef gen1
        let (newApGrid, newAcGrid, gen2) = initGame num
        drawGame (concat newApGrid) 9 9 canvas 0 "P" gameStatusRef
        liftIO $ writeIORef apGridRef newApGrid
        liftIO $ writeIORef acGridRef newAcGrid

        let numFlagged = getNumFlags newApGrid
        let numMines = getNumMines newAcGrid
        element mineCountDisp # set UI.text ("Mines: " ++ show (numMines-numFlagged))

        liftIO $ writeIORef gameStatusRef "P"

    on UI.click makeMove $ \_ -> do
        apGrid <- liftIO $ readIORef apGridRef
        acGrid <- liftIO $ readIORef acGridRef
        let newGrid = aiMove apGrid acGrid
        updateNumMines newGrid acGrid mineCountDisp
        liftIO $ writeIORef apGridRef newGrid

        endGame newGrid acGrid canvas gameStatusRef mineCountDisp

    on UI.click makeNonObvs $ \_ -> do
        apGrid <- liftIO $ readIORef apGridRef
        acGrid <- liftIO $ readIORef acGridRef
        let (newGrid, ref) = aiNonObviousMove apGrid acGrid
        updateNumMines newGrid acGrid mineCountDisp
        liftIO $ writeIORef apGridRef newGrid

        element display # set UI.text (show ref)

        endGame newGrid acGrid canvas gameStatusRef mineCountDisp


    on UI.mousemove canvas $ \xy ->
        do liftIO $ writeIORef mousePos xy

    on UI.contextmenu canvas $ \xy -> do
        state <- liftIO $ readIORef gameStatusRef
        if state == "W" || state == "L" then do
            return ()
            else do
                let coord = convCoord xy 9 9
                apGrid <- liftIO $ readIORef apGridRef
                acGrid <- liftIO $ readIORef acGridRef
                let newGrid = handleInput2 apGrid acGrid coord "F"
                updateNumMines newGrid acGrid mineCountDisp

                liftIO $ writeIORef apGridRef newGrid
                drawGame (concat newGrid) 9 9 canvas 0 "P" gameStatusRef


    on UI.click canvas $ \_ -> do
        state <- liftIO $ readIORef gameStatusRef
        if state == "W" || state == "L" then do
            return ()
            else do
                xy <- liftIO $ readIORef mousePos
                let coord = convCoord xy 9 9
                apGrid <- liftIO $ readIORef apGridRef
                acGrid <- liftIO $ readIORef acGridRef
                let newGrid = handleInput2 apGrid acGrid coord ""
                liftIO $ writeIORef apGridRef newGrid

                -- let (c,m) = markNonObvious newGrid
                -- element display # set UI.text (show c)
                -- element d2 # set UI.text (show m)
                -- element d2 # set UI.text (show r)
                
                endGame newGrid acGrid canvas gameStatusRef mineCountDisp

    return ()


endGame apGrid acGrid canvas gameStatusRef mineCountDisp = do
    let status = checkEndgame apGrid acGrid
    -- drawGame (concat newGrid) 9 9 canvas 0 status gameStatusRef
    if status == "W" then do
        let finishGrid = flagAllUnmarked apGrid
        drawGame (concat finishGrid) 9 9 canvas 0 status gameStatusRef
        updateNumMines finishGrid acGrid mineCountDisp
        else do 
            drawGame (concat apGrid) 9 9 canvas 0 status gameStatusRef


-- updateNumMines :: ApparentGrid -> ActualGrid -> 
updateNumMines apGrid acGrid disp = do
    let numFlagged = getNumFlags apGrid
    let numMines = getNumMines acGrid
    element disp # set UI.text ("Mines: "++ show (numMines-numFlagged))
    return ()

convCoord :: (Double, Double) -> Int -> Int -> (Int, Int)
convCoord (col,row) maxR maxC = 
    let x = floor $ row / (fromIntegral $ canvasHeight `div` 9)
        y = floor $ col / (fromIntegral $ canvasWidth `div` 9)
    in (min x (maxR-1) , min y (maxC-1))


-- can calculate width and height from grid
drawGame :: [Int] -> Int -> Int -> Element -> Int -> String -> IORef String ->  UI ()
drawGame [] _ _ _ _ _ _ = return ()
drawGame xs h w canvas index "W" stateRef = do
    drawSquares xs h w canvas index
    liftIO $ writeIORef stateRef "W"
    drawWin canvas
drawGame xs h w canvas index "L" stateRef = do 
    drawSquares xs h w canvas index
    liftIO $ writeIORef stateRef "L"
    drawLoss canvas 
drawGame xs h w canvas index _ _ = do
    canvas # set' UI.fillStyle (UI.htmlColor "black")
    canvas # UI.fillRect
        (0,0) (fromIntegral canvasWidth) (fromIntegral canvasHeight)

    drawSquares xs h w canvas (index)

drawLoss canvas = do
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # set' UI.textAlign (UI.Center)
    canvas # set' UI.textFont "100px sans-serif"
    canvas # UI.fillText
        ("You lose :(")
        (fromIntegral $ canvasWidth `div` 2, 
            fromIntegral $ canvasHeight `div` 2)
    

drawWin canvas = do
    canvas # set' UI.fillStyle (UI.htmlColor "lime")
    canvas # set' UI.textAlign (UI.Center)
    canvas # set' UI.textFont "100px sans-serif"
    canvas # UI.fillText
        ("You won!")
        (fromIntegral $ canvasWidth `div` 2, 
            fromIntegral $ canvasHeight `div` 2)
    

drawSquares :: [Int] -> Int -> Int -> Element -> Int -> UI ()
drawSquares [] _ _ _ _ = return ()
drawSquares (x:xs) h w canvas index = do
    drawSquare x h w canvas index
    drawSquares xs h w canvas (index+1)

drawSquare :: Int -> Int -> Int -> Element -> Int -> UI ()
drawSquare val h w canvas index = do
    -- different spaces based on val
    let (spaceVal, spaceColour, numColour) = valOf val

    let i = index `div` w
    let j = index `mod` w
    canvas # set' UI.fillStyle (UI.htmlColor spaceColour)
    canvas # UI.fillRect
        ( fromIntegral ((j * (canvasWidth `div` w) + 3))
        , fromIntegral ((i * (canvasHeight `div` h) + 3))
        )
        (fromIntegral (canvasWidth `div` w - 6))
        (fromIntegral (canvasHeight `div` h - 6))
    canvas # set' UI.fillStyle (UI.htmlColor numColour)
    canvas # set' UI.textAlign (UI.Center)
    canvas # set' UI.textFont "50px sans-serif"
    canvas # UI.fillText
        (spaceVal)
        -- (show val)
        ( fromIntegral ((j * (canvasWidth `div` w) + 
            fromIntegral (canvasWidth `div` w ) `div` 2))
        , fromIntegral ((i * (canvasHeight `div` h) +
            (fromIntegral (canvasHeight `div` h + 35) `div` 2)))
        )

valOf :: Int -> (String,String,String)
valOf v = 
    case v of
        0 -> ("", "grey","white")
        (-1) -> ("*", "red", "black")
        10 -> ("🚩", "grey", "red")
        1 -> ("1", "grey", "blue")
        2 -> ("2", "grey", "green")
        3 -> ("3", "grey", "red")
        4 -> ("4", "grey", "purple")
        5 -> ("5", "grey", "DarkRed")
        6 -> ("6", "grey", "turquoise")
        7 -> ("7", "grey", "black")
        8 -> ("8", "grey", "silver")
        9 -> ("", "white", "grey")
        _ -> (show v, "grey", "white")

setNoContextMenu :: JSFunction ()
setNoContextMenu = ffi "document.getElementById('canvas').setAttribute('oncontextmenu', 'return false;')"

test = rref [[1,1,0,0,0,1]
            ,[1,1,1,0,0,1]
            ,[0,1,1,0,0,1]
            ,[0,0,1,1,0,1]
            ,[0,0,1,1,1,1]
            ,[0,0,0,1,1,1]]