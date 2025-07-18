module Main where
import Control.Concurrent (threadDelay)
import Text.Printf (printf)
import System.IO (hFlush, stdout)


main :: IO ()
main = do
    putStrLn "Starting Pomodoro Timer (25 minutes)..."
    workTimer 5
    breakTimer 2

-- generic timer
timer :: Int -> IO ()
timer 0 = do
    putStrLn "\r00:00 remaining"
    putStrLn "Time's up!"
timer n = do
    let mins = n `div` 60
    let secs = n `mod` 60
    printf "\r%02d:%02d remaining" mins secs
    hFlush stdout
    threadDelay 1000000  -- 1 second
    timer (n - 1)


-- work timer
workTimer :: Int -> IO ()
workTimer x = do
    timer x
    putStrLn "Work session complete! Take a break."

-- break timer
breakTimer :: Int -> IO ()
breakTimer x = do
    timer x
    putStrLn "Break session complete! Back to work."
            
