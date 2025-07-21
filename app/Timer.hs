module Timer (timer) where

import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import Data.IORef
import System.Console.ANSI (cursorUpLine, clearLine)



--generic timer
timer :: Int -> IORef Bool -> IORef Bool -> IO ()
timer 0 _ _ = do
    putStrLn "\r00:00 remaining"
    putStrLn "Time's up!"
timer n pauseFlag skipFlag = do
    paused <- readIORef pauseFlag
    skip <- readIORef skipFlag

    if skip then do
        clearLines (catHeight + 1)
        putStrLn "\nSkipped remaining time."
        writeIORef skipFlag False
        return ()
    else if paused then do
        threadDelay 500000
        timer n pauseFlag skipFlag
    else do
        let mins = n `div` 60
        let secs = n `mod` 60
        printf "\r%02d:%02d remaining" mins secs
        hFlush stdout
        threadDelay 1000000
        timer (n - 1) pauseFlag skipFlag
    
catHeight :: Int
catHeight = 2

clearLines :: Int -> IO ()
clearLines 0 = return ()
clearLines n = do
    clearLine
    cursorUpLine 1
    clearLines (n - 1)