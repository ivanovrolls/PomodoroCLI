module Main where
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Text.Printf (printf)
import System.IO (hFlush, stdout, stdin, hSetBuffering, BufferMode(NoBuffering), hGetChar)
import System.Console.ANSI (cursorUpLine, cursorDownLine, clearLine)
import System.Environment (getArgs)
import Text.Read (readMaybe)
import Data.IORef
import Control.Monad (when, forever)



main :: IO ()
main = do
    args <- getArgs
    case args of
        [workStr, breakStr] -> do
            case (readMaybe workStr :: Maybe Int, readMaybe breakStr :: Maybe Int) of
                (Just workTime, Just breakTime) -> do
                    putStrLn $ "Starting Pomodoro Timer with " ++ show workTime ++ " minutes of work and " ++ show breakTime ++ " minutes of break."
                    workTimer (workTime * 60)
                    breakTimer (breakTime * 60)
                _ -> putStrLn "Please provide valid integers for work and break times."
        _ -> putStrLn "Usage: PomodoroCLI <work_time_in_minutes> <break_time_in_minutes>"


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


--work timer
workTimer :: Int -> IO ()
workTimer x = do
    pauseRef <- newIORef False
    _ <- forkIO $ inputListener pauseRef --start input listener in a separate thread
    done <- newEmptyMVar
    doneTimer <- newEmptyMVar

    _ <- forkIO $ do
        animate workFrames (x * 2) pauseRef --roughly x seconds
        putMVar done ()

    _ <- forkIO $ do
        timer x pauseRef
        putMVar doneTimer ()

    takeMVar done
    takeMVar doneTimer
    putStrLn "\nWork session complete! Time for a break."

--break timer
breakTimer :: Int -> IO ()
breakTimer x = do
    pauseRef <- newIORef False
    _ <- forkIO $ inputListener pauseRef
    done <- newEmptyMVar
    doneTimer <- newEmptyMVar

    _ <- forkIO $ do
        animate sleepFrames (x * 2) pauseRef --roughly x seconds
        putMVar done ()

    _ <- forkIO $ do
        timer x pauseRef
        putMVar doneTimer ()

    takeMVar done
    takeMVar doneTimer
    putStrLn "\nBreak session complete! Back to work."


clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

--cat height in lines
catHeight :: Int
catHeight = 3

cursorUpN :: Int -> IO ()
cursorUpN n = cursorUpLine n


animate :: [String] -> Int -> IORef Bool -> IO () --animates frames
animate _ 0 _ = return ()
animate frames n pauseRef = do
    paused <- readIORef pauseRef
    if paused
    then do
        threadDelay 500000 --if paused, just wait
        animate frames n pauseRef
    else do
        cursorUpN catHeight --move cursor up by height of cat
        sequence_ [clearLine >> cursorDownLine 1 | _ <- [1..catHeight]] --clear line then move down (recurses and clears the lines)
        cursorUpN catHeight
        putStr (frames !! (n `mod` length frames)) --print current frame
        hFlush stdout --flush output to avoid buffering issues
        threadDelay 500000  --0.5s
        animate frames (n - 1) pauseRef --recursively animate n amount of times

--listen for input
inputListener :: IORef Bool -> IO ()
inputListener pauseFlag = do
    hSetBuffering stdin NoBuffering --set input so u dont have to press enter
    forever $ do
        c <- hGetChar stdin
        when (c == 'p' || c == 'P') $ do
            paused <- readIORef pauseFlag
            writeIORef pauseFlag (not paused) --toggle pause state
            putStrLn $ if paused then "\nResumed." else "\nPaused."


--sleep
sleepFrames :: [String]
sleepFrames =
  [ unlines
      [ " /\\_/\\  "
      , "( -.- ) zZ"
      , " > ^ <"
      ]
  , unlines
      [ " /\\_/\\  "
      , "( o.o ) zZ"
      , " > ^ <"
      ]
  ]

--working
workFrames :: [String]
workFrames =
  [ unlines
      [ " /\\_/\\  "
      , "( -.- )  "
      , " >⌨️<  "   --typing on keyboard
      ]
  , unlines
      [ " /\\_/\\  "
      , "( o.o )  "
      , "  >⌨️<  "
      ]
  , unlines
      [ " /\\_/\\  "
      , "( -.- )  "
      , "  \\_/   "  --stretching
      ]
  , unlines
      [ " /\\_/\\  "
      , "( ^.^ )  "
      , "  >☕<   "  --sipping coffee
      ]
  ]

