module Timer (timer) where

import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import System.Console.ANSI (cursorUpLine, clearLine)
import Data.IORef
import Control.Concurrent.MVar (tryTakeMVar, MVar)

timer :: Int -> IORef Bool -> MVar Bool -> IORef Bool -> IO ()
timer 0 _ _ _ = do
    putStrLn "\r00:00 remaining"
    putStrLn "Time's up!"
timer n pauseFlag skipSignal skippedRef = do
    skipTaken <- tryTakeMVar skipSignal
    case skipTaken of
        Just _ -> writeIORef skippedRef True
        Nothing -> return ()

    skipped <- readIORef skippedRef
    if skipped then do
        clearLines (catHeight)
        putStrLn "\nSkipped remaining time."
    else do
        paused <- readIORef pauseFlag
        if paused then do
            threadDelay 500000
            timer n pauseFlag skipSignal skippedRef
        else do
            let mins = n `div` 60
            let secs = n `mod` 60
            printf "\r%02d:%02d remaining" mins secs
            hFlush stdout
            threadDelay 1000000
            timer (n - 1) pauseFlag skipSignal skippedRef

catHeight :: Int
catHeight = 3

clearLines :: Int -> IO ()
clearLines 0 = return ()
clearLines n = do
    clearLine
    if n > 1 then cursorUpLine 1 else return ()
    clearLines (n - 1)

