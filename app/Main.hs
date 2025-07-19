module Main where
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Text.Printf (printf)
import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode(NoBuffering), hGetChar)
import System.Console.ANSI (cursorUpLine, cursorDownLine, clearLine)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.IORef
import Control.Monad (when, forever)
import Animation (animate, cursorUpN, catHeight, sleepFrames, workFrames)
import Timer (timer)
import Input (inputListener)
import Session (runSession)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [workStr, breakStr] -> do
            case (readMaybe workStr :: Maybe Int, readMaybe breakStr :: Maybe Int) of
                (Just workTime, Just breakTime) -> do
                    putStrLn $ "Starting Pomodoro Timer with " ++ show workTime ++ " minutes of work and " ++ show breakTime ++ " minutes of break."
                    runSession (workTime * 60) workFrames "\nWork session complete! Time for a break."
                    runSession (breakTime * 60) sleepFrames "\nBreak session complete! Back to work."
                _ -> putStrLn "Please provide valid integers for work and break times."
        _ -> putStrLn "Usage: PomodoroCLI <work_time_in_minutes> <break_time_in_minutes>"