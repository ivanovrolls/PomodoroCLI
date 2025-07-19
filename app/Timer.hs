module Timer (timer) where

import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import Data.IORef


--generic timer
timer :: Int -> IORef Bool -> IO ()
timer 0 _ = do
    putStrLn "\r00:00 remaining"
    putStrLn "Time's up!"
timer n pauseRef = do
    paused <- readIORef pauseRef
    if paused
    then do
        threadDelay 500000
        timer n pauseRef
    else do
        let mins = n `div` 60
        let secs = n `mod` 60
        printf "\r%02d:%02d remaining" mins secs
        hFlush stdout
        threadDelay 1000000
        timer (n - 1) pauseRef