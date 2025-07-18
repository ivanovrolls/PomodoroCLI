module Main where
import Control.Concurrent (forkIO, threadDelay, newEmptyMVar, putMVar, takeMVar)
import Text.Printf (printf)
import System.IO (hFlush, stdout)
import System.Console.ANSI (cursorUpLine, cursorDownLine, clearLine)

main :: IO ()
main = do
    putStrLn "Starting Pomodoro Timer (25 minutes)..."
    workTimer 5
    breakTimer 5

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
    done <- newEmptyMVar

    --animation thread
    _ <- forkIO $ do
        animate workFrames (x * 2) --roughly x seconds
        putMVar done ()

    --timer thread
    timer x 

    --wait for animation and timer
    takeMVar done
    putStrLn "\nWork session complete! Time for a break."

-- break timer
breakTimer :: Int -> IO ()
breakTimer x = do
    done <- newEmptyMVar

    --animation thread
    _ <- forkIO $ do
        animate sleepFrames (x * 2) --roughly x seconds
        putMVar done ()

    --timer thread
    timer x

    --wait for animation and timer
    takeMVar done
    putStrLn "\nBreak session complete! Back to work."


clearScreen :: IO ()
clearScreen = putStr "\ESC[2J\ESC[H"

--cat height in lines
catHeight :: Int
catHeight = 3

cursorUpN :: Int -> IO ()
cursorUpN n = cursorUpLine n


animate :: [String] -> Int -> IO () --animates frames
animate _ 0 = return ()
animate frames n = do
    cursorUpN catHeight --move cursor up by height of cat
    sequence_ [clearLine >> cursorDownLine 1 | _ <- [1..catHeight]] --clear line then move down (recurses and clears the lines)
    cursorUpN catHeight
    putStr (frames !! (n `mod` length frames)) --print current frame
    hFlush stdout --flush output to avoid buffering issues
    threadDelay 500000  --0.5s
    animate frames (n - 1) --recursively animate n amount of times

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

