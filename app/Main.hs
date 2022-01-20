{-#LANGUAGE ScopedTypeVariables#-}

-- **************  Simple ADA BackTest Strategy Project  ****************
-- ****  Load csv 2021 daily candles data from Yahoo! Finance for ADA
-- ****  Allow user to input down% and up% parameters to back test
-- ****         user enters 0 tu stop running tests
-- ****  use reader to keep history loaded from Yahoo finance csv file
-- ****  use state to keep track of best run
-- ****  use writer to keep track of all runs
-- ****  at the end save all runs results in file log.txt
-- ****  
-- ****  by Pedro Doren
-- ****  

module Main where

import Text.CSV (parseCSVFromFile)
import Data.List
import System.Environment
import Control.Monad.RWS

import Helpers

main :: IO ()
main = do
    args <- getArgs

    putStrLn "\n\n------------------------------------"
    putStrLn "Simple ADA BackTest Strategy Project"
    putStrLn "------------------------------------\n"

    loaded_file <- parseCSVFromFile "data210902.csv"

    let
        noEmptyRows = either (const []) (filter (\row -> 2 <= length row))
        candles' = noEmptyRows loaded_file
        candles = map (map (\s -> read(s)::Float)) candles'
        startState = [0::Float, 0::Float, 0::Float]
    (bestRun, allRuns) <- execRWST (execRun 1) candles startState
    putStrLn "\n----- Logs -----------------------------\n"
    putStr "[State or besRun]  : "
    print bestRun
    putStrLn "[Write or allRuns] : "
    print allRuns
    writeFile "log.txt" (show allRuns)
    putStrLn ">>>> Saved on log.txt"
    
execRun :: Int -> RWST [[Float]] [[Float]] [Float] IO ()
execRun i =
    do
        history <- ask        -- get candle history from reader Env

        lift $ putStrLn $ "\nRun #  " ++ show (i)
        lift $ putStrLn "Up percentage (1-100%): "
        sup  <- lift getLine
        let up = read sup
        lift $ putStrLn "Down percentage (1-100%) [0 to exit]: "
        sdown <- lift getLine
        let down = read sdown

        let startPosition = [1000::Float, 0::Float, getOpen (head history), 0::Float, 0::Float, 0::Float]
            result = (last (scanl (calcPosition up down) startPosition history) !! 5 - 1000.0)/10.0
        lift $ putStr $ "Up = " ++ show(up) ++ "%, Down = " ++ show(down) ++ "%"
        lift $ putStrLn $ " --> ROI = " ++ show (result) ++ "%"

        let newRunScore = [up, down, result]
        bestRunSoFar <- get
        if (newRunScore !! 2) > (bestRunSoFar !! 2) then put newRunScore
        else put bestRunSoFar    
            
        if down > 0 then do
            tell $ [newRunScore] 
            (execRun  (i + 1))
        else return ()
        