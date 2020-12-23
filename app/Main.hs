module Main where

import qualified Graphics.UI.Threepenny as UI
import Graphics.UI.Threepenny.Core

import Reactive.Threepenny
import Minesweeper
-- import ApparentBoard

-- main :: IO ()
-- main = do
--     gen <- newStdGen
--     let randomNumbers = randomRs (0, size-1) gen :: [Int]
--     let game = initialiseGame randomNumbers
--     startGUI defaultConfig (setup game randomNumbers)

-- setup
canvasHeight :: Int
canvasHeight = 600

canvasWidth :: Int
canvasWidth = 600

main :: IO ()
main = do
    -- init game in here giving an apparent board
    startGUI defaultConfig setup

setup :: Window -> UI ()
setup window = do
    return window # set title "Minesweeper"

    -- TODO: set width and height based on size of grid
    canvas <- UI.canvas 
        # set UI.width 600 
        # set UI.height 600 
        # set UI.style [("border", "solid black 1px"), ("background", "#000")]

    -- openMode <- UI.button #+ [string "Open"]
    -- markMode <- UI.button #+ [string "Mark"]
    newGame <- UI.button #+ [string "Start New Game"]

    drawGame (concat (replicate 10 (replicate 10 0))) 10 10 canvas 0

    getBody window #+
        [column [element canvas]
        , element newGame]

    return ()

-- can calculate width and height from grid
drawGame :: [Int] -> Int -> Int -> Element -> Int -> UI ()
drawGame [] _ _ _ _ = return ()
drawGame (x:xs) h w canvas index = do
    drawSquare x h w canvas index
    drawGame xs h w canvas (index+1)

drawSquare :: Int -> Int -> Int -> Element -> Int -> UI ()
drawSquare val h w canvas index = do
    -- different spaces based on val I assume
    let i = index `div` w
    let j = index `mod` w
    canvas # set' UI.fillStyle (UI.htmlColor "gray")
    canvas # UI.fillRect
        ( fromIntegral ((j * (canvasWidth `div` w) + 3))
        , fromIntegral ((i * (canvasHeight `div` h) + 3))
        )
        (fromIntegral (canvasWidth `div` w - 6))
        (fromIntegral (canvasHeight `div` h - 6))


-- getSpaces :: Int -> [UI Element]
-- getSpaces x = replicate x $ getSpace

-- getSpace :: UI Element
-- getSpace = UI.button # set text "test"
