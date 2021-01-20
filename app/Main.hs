module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core
import Data.Time.Clock.POSIX
import Reactive.Threepenny
import System.Random
import Minesweeper
import AIPlayer
import Data.Time
import Control.Applicative
import Data.Matrix

import Data.IORef
import Control.Monad.Trans (liftIO)

-----------
-- SETUP --
-----------
boxSize :: Int
boxSize = 36

canvasHeight :: Int
canvasHeight = boxSize * heightC

canvasWidth :: Int
canvasWidth = boxSize * widthC

startMines :: Int
startMines = 40

heightC :: Int
heightC = 16

widthC :: Int
widthC = 16

-- TODO
-- maybe have some predefined difficulty levels?
-- maybe consolidate into make move button?
-- maybe first move def not mine?

main :: IO ()
main = do
    -- Generating random numbers was a slight problem.
    -- Reloading the page does not run 'main' again, only 'setup'
    -- so reloading the page does not get a new board - only
    -- the button 'New Board' gets a new board. Or rerunning the 
    -- program (stack exec minesweeper)
    
    -- StdGen initialised with time so different each time the game is executed
    epoch_int <- (read <$> formatTime defaultTimeLocale "%s" <$> getCurrentTime) :: IO Int

    -- Gets initial apGrid, acGrid and StdGen
    let boardstate = initGame epoch_int startMines widthC heightC
    startGUI defaultConfig (setup boardstate)

setup :: (ApparentGrid, ActualGrid, StdGen) -> Window -> UI ()
setup (apparentGrid, actualGrid, g1) window = do
    return window # set title "Minesweeper"

    -- Setup IORefs
    apGridRef <- liftIO $ newIORef apparentGrid
    acGridRef <- liftIO $ newIORef actualGrid
    genRef <- liftIO $ newIORef g1
    -- necessary to distinguish right vs left clicks
    mousePos <- liftIO $ newIORef (0,0)
    gameStatusRef <- liftIO $ newIORef "P"

    -- For debugging / displaying info from program
    display <- UI.span # set text "empty"
    d2 <- UI.span # set text "empty"
    d3 <- UI.span # set text "empty"

    -- Mine Counter
    mineCountDisp <- UI.span # set text ("Mines: " ++ show (getNumMines actualGrid))

    -- Canvas where the mine grid is interacted with
    canvas <- UI.canvas 
        # set UI.width (canvasWidth)
        # set UI.height (canvasHeight)
        # set UI.style [("border", "solid black 1px"), ("background", "#000")]
        # set UI.id_ "canvas"

    -- Option buttons
    newGame <- UI.button #+ [string "New Board"]
    makeMove <- UI.button #+ [string "Make Obvious Move"]
    makeNonObvs <- UI.button #+ [string "Make Non-Obvious Move"]
    makeProbMove <- UI.button #+ [string "Guess Move"]

    ag <- liftIO $ readIORef apGridRef
    -- Draw game in mode "Playing"
    drawGame (concat ag) canvas 0 "P" gameStatusRef

    -- Add elements to the HTML page
    getBody window #+
        [ column [element canvas]
        , element newGame, element makeMove, element makeNonObvs, element makeProbMove]
    getBody window #+ [row [element mineCountDisp],
                        row [element display],
                        row [element d2],
                        row [element d3]]

    -- Uses FFI to turn off the context menu on right click in the canvas
    runFunction setNoContextMenu


    ---------------------
    -- Button Handlers --
    ---------------------

    on UI.click newGame $ \_ -> do
        -- Generate random num
        gen <- liftIO $ readIORef genRef
        let (num, gen1) = random gen
        liftIO $ writeIORef genRef gen1

        -- Init new game with new random number
        let (newApGrid, newAcGrid, gen2) = initGame num startMines widthC heightC
        drawGame (concat newApGrid) canvas 0 "P" gameStatusRef
        liftIO $ writeIORef apGridRef newApGrid
        liftIO $ writeIORef acGridRef newAcGrid

        -- Set Mine counter
        let numFlagged = getNumFlags newApGrid
        let numMines = getNumMines newAcGrid
        element mineCountDisp # set UI.text ("Mines: " ++ show (numMines-numFlagged))

        -- Write "Playing" game status 
        liftIO $ writeIORef gameStatusRef "P"


    -- AI Make Obvious Move
    on UI.click makeMove $ \_ -> do
        -- See what state we're in
        state <- liftIO $ readIORef gameStatusRef
        if state == "W" || state == "L" then do
            return ()
            else do
                -- If game still playing try make obvious move
                apGrid <- liftIO $ readIORef apGridRef
                acGrid <- liftIO $ readIORef acGridRef
                
                let numMines = getNumMines acGrid
                let numFlagged = getNumFlags apGrid

                -- aiObviousMove returns a new ApparentGrid which is then drawn
                let newGrid = aiObviousMove apGrid acGrid (numMines-numFlagged)

                -- Update the mine counter
                updateNumMines newGrid acGrid mineCountDisp
                liftIO $ writeIORef apGridRef newGrid

                -- Check for endgame
                endGame newGrid acGrid canvas gameStatusRef mineCountDisp

    -- Slightly more sophisticated AI move generator using Matrices to find moves
    on UI.click makeNonObvs $ \_ -> do
        state <- liftIO $ readIORef gameStatusRef
        if state == "W" || state == "L" then do
            return ()
            else do
                apGrid <- liftIO $ readIORef apGridRef
                acGrid <- liftIO $ readIORef acGridRef
                let (newGrid, ref, toFlag, toOpen) = aiNonObviousMove apGrid acGrid
                updateNumMines newGrid acGrid mineCountDisp
                liftIO $ writeIORef apGridRef newGrid

                -- Displays some information 
                --      the coords it found with mines and definitely without mines
                --      the Reduced Row Echelon Matrix
                element display # set UI.text ("Coords to flag " ++ show toFlag)
                element d2 # set UI.text ("Coords to open " ++ show toOpen)
                element d3 # set UI.text ("rref " ++ show ref)

                endGame newGrid acGrid canvas gameStatusRef mineCountDisp

    -- Guesses a move based on very naive probability that works some of the time
    on UI.click makeProbMove $ \_ -> do
        state <- liftIO $ readIORef gameStatusRef
        if state == "W" || state == "L" then do
            return ()
            else do
                apGrid <- liftIO $ readIORef apGridRef
                acGrid <- liftIO $ readIORef acGridRef
                let numMines = getNumMines acGrid
                let numFlagged = getNumFlags apGrid

                -- Again returns a new ApparentGrid
                -- let newGrid = probMove apGrid acGrid (numMines-numFlagged)
                let (newGrid, probs, ctoopen) = probMove apGrid acGrid (numMines-numFlagged)
                updateNumMines newGrid acGrid mineCountDisp
                liftIO $ writeIORef apGridRef newGrid

                element display # set UI.text ("probs " ++ show probs)
                element d2 # set UI.text ("coord " ++ show ctoopen)

                endGame newGrid acGrid canvas gameStatusRef mineCountDisp

                -- let numMines = getNumMines acGrid
                -- let numFlagged = getNumFlags apGrid
                -- let poss = enumerate apGrid (numMines - numFlagged)
                -- element display # set UI.text (show poss)
                -- return ()


    -- Keep track of the mouse position in the canvas
    on UI.mousemove canvas $ \xy ->
        do liftIO $ writeIORef mousePos xy

    -- Use right click to flag
    on UI.contextmenu canvas $ \xy -> do
        state <- liftIO $ readIORef gameStatusRef
        if state == "W" || state == "L" then do
            return ()
            else do
                -- Convert canvas coords to cell coordinates
                let coord = convCoord xy

                apGrid <- liftIO $ readIORef apGridRef
                acGrid <- liftIO $ readIORef acGridRef

                -- Take the coordinate and "Flag" it
                let newGrid = handleInput apGrid acGrid coord "F"
                updateNumMines newGrid acGrid mineCountDisp

                liftIO $ writeIORef apGridRef newGrid
                drawGame (concat newGrid) canvas 0 "P" gameStatusRef

    -- Left click to open squares
    on UI.click canvas $ \_ -> do
        state <- liftIO $ readIORef gameStatusRef
        if state == "W" || state == "L" then do
            return ()
            else do
                xy <- liftIO $ readIORef mousePos

                let coord = convCoord xy
                apGrid <- liftIO $ readIORef apGridRef
                acGrid <- liftIO $ readIORef acGridRef

                let newGrid = handleInput apGrid acGrid coord ""
                liftIO $ writeIORef apGridRef newGrid

                endGame newGrid acGrid canvas gameStatusRef mineCountDisp

    return ()

-- Check endgame scerario
endGame :: ApparentGrid -> ActualGrid -> Element -> IORef String -> Element -> UI ()
endGame apGrid acGrid canvas gameStatusRef mineCountDisp = do
    let status = checkEndgame apGrid acGrid
    if status == "W" then do

        -- If win then flag all the unmarked cells as they must be mines 
        let finishGrid = flagAllUnmarked apGrid
        drawGame (concat finishGrid) canvas 0 status gameStatusRef
        liftIO $ writeIORef gameStatusRef "W"
        updateNumMines finishGrid acGrid mineCountDisp
        else do 
            if status == "L" then do

                -- If loss then reveal the positions of mines
                let newGrid = revealMines apGrid acGrid
                liftIO $ writeIORef gameStatusRef "L"
                drawGame (concat newGrid) canvas 0 status gameStatusRef
                else do
                    drawGame (concat apGrid) canvas 0 status gameStatusRef

-- Reveal mine positions on game loss
revealMines :: ApparentGrid -> ActualGrid -> ApparentGrid
revealMines apGrid acGrid =
    let mines = getMineCoords acGrid
    in openMultipleSquares apGrid acGrid mines

-- Update mine counter
updateNumMines :: ApparentGrid -> ActualGrid -> Element -> UI () 
updateNumMines apGrid acGrid disp = do
    let numFlagged = getNumFlags apGrid
    let numMines = getNumMines acGrid
    element disp # set UI.text ("Mines: "++ show (numMines-numFlagged))
    return ()

-- Convert canvas screen coord to game coord
convCoord :: (Double, Double) -> (Int, Int)
convCoord (col,row) = do
    let x = floor $ row / (fromIntegral boxSize)
    let y = floor $ col / (fromIntegral boxSize)
    let maxR = (canvasHeight) `div` boxSize 
    let maxC = (canvasWidth) `div` boxSize
    (min x (maxR-1) , min y (maxC-1))


-- Game drawing function
-- Take a concatenated 2D grid (ApparentGrid)
drawGame :: [Int] -> Element -> Int -> String -> IORef String -> UI ()
drawGame [] _ _ _ _ = return ()

-- Win = show win
drawGame xs canvas index "W" stateRef = do
    drawSquares xs canvas index
    liftIO $ writeIORef stateRef "W"
    drawWin canvas 

-- Loss = show loss
drawGame xs canvas index "L" stateRef= do 
    drawSquares xs canvas index 
    liftIO $ writeIORef stateRef "L"
    drawLoss canvas 

-- Else draw game
drawGame xs canvas index _ _ = do
    canvas # set' UI.fillStyle (UI.htmlColor "black")
    canvas # UI.fillRect
        (0,0) (fromIntegral canvasWidth) (fromIntegral canvasHeight)

    drawSquares xs canvas (index)

-- Draw losing message
drawLoss :: UI.Canvas-> UI ()
drawLoss canvas = do
    canvas # set' UI.fillStyle (UI.htmlColor "red")
    canvas # set' UI.textAlign (UI.Center)
    canvas # set' UI.textFont "100px sans-serif"
    canvas # UI.fillText
        ("You lose :(")
        (fromIntegral $ canvasWidth `div` 2, 
            fromIntegral $ canvasHeight `div` 2)
    
-- Draw winning message
drawWin :: UI.Canvas -> UI ()
drawWin canvas = do
    canvas # set' UI.fillStyle (UI.htmlColor "lime")
    canvas # set' UI.textAlign (UI.Center)
    canvas # set' UI.textFont "100px sans-serif"

    canvas # UI.fillText
        ("You won!")
        (fromIntegral $ canvasWidth `div` 2, 
            fromIntegral $ canvasHeight `div` 2)
    

-- Draw cells
drawSquares :: [Int] -> Element -> Int-> UI ()
drawSquares [] _ _ = return ()
drawSquares (x:xs) canvas index = do
    drawSquare x canvas index
    drawSquares xs canvas (index+1)

-- Squares size is always constant
-- Dimensions change according to heightC and widthC
drawSquare :: Int -> Element -> Int -> UI ()
drawSquare val canvas index = do
    let height = canvasHeight
    let width = canvasWidth
    let dimX = height `div` boxSize
    let dimY = width `div` boxSize 
    let (spaceVal, spaceColour, numColour) = valOf val

    let i = index `div` dimY
    let j = index `mod` dimY

    canvas # set' UI.fillStyle (UI.htmlColor spaceColour)
    canvas # UI.fillRect
        ( fromIntegral ((j * (width `div` dimY) + 3))
        , fromIntegral ((i * (height `div` dimX) + 3))
        )
        (fromIntegral $ boxSize - 6)
        (fromIntegral $ boxSize - 6)

    canvas # set' UI.fillStyle (UI.htmlColor numColour)
    canvas # set' UI.textAlign (UI.Center)
    canvas # set' UI.textFont "25px sans-serif"
    canvas # UI.fillText
        (spaceVal)
        -- (show val)
        ( fromIntegral ((j * (width `div` dimY) + 
            fromIntegral (width `div` dimY ) `div` 2))
        , fromIntegral ((i * (height `div` dimX) +
            (fromIntegral (height `div` dimX + 20) `div` 2)))
        )

-- Convert 2D Int values to minesweeper values + colour
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

-- Disable context menu popping up when flagging a cell
setNoContextMenu :: JSFunction ()
setNoContextMenu = ffi "document.getElementById('canvas').setAttribute('oncontextmenu', 'return false;')"