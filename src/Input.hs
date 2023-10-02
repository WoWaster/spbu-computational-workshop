module Input (inputInt, inputDouble) where

import System.IO
import Text.Read

prompt :: String -> IO String
prompt text = do
    putStr text
    hFlush stdout
    getLine

inputInt :: String -> IO Int
inputInt msg = do
    numStr <- prompt msg
    case readMaybe numStr of
        Nothing -> do putStrLn "Введено не целое число"; inputInt msg
        Just num -> return num

inputDouble :: String -> IO Double
inputDouble msg = do
    numStr <- prompt msg
    case readMaybe numStr of
        Nothing -> do putStrLn "Введено не вещественное число"; inputDouble msg
        Just num -> return num
