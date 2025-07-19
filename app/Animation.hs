module Animation
  ( animate
  , cursorUpN
  , catHeight
  , sleepFrames
  , workFrames
  ) where

import System.Console.ANSI (cursorUpLine, cursorDownLine, clearLine)
import System.IO (hFlush, stdout)
import Control.Concurrent (threadDelay)
import Data.IORef

catHeight :: Int
catHeight = 3

cursorUpN :: Int -> IO ()
cursorUpN n = cursorUpLine n

animate :: [String] -> Int -> IORef Bool -> IO ()
animate _ 0 _ = return ()
animate frames n pauseRef = do
    paused <- readIORef pauseRef
    if paused
    then do
        threadDelay 500000
        animate frames n pauseRef
    else do
        cursorUpN catHeight
        sequence_ [clearLine >> cursorDownLine 1 | _ <- [1..catHeight]]
        cursorUpN catHeight
        putStr (frames !! (n `mod` length frames))
        hFlush stdout
        threadDelay 500000
        animate frames (n - 1) pauseRef

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

workFrames :: [String]
workFrames =
  [ unlines
      [ " /\\_/\\  "
      , "( -.- )  "
      , " >⌨️<  "
      ]
  , unlines
      [ " /\\_/\\  "
      , "( o.o )  "
      , "  >⌨️<  "
      ]
  , unlines
      [ " /\\_/\\  "
      , "( -.- )  "
      , "  \\_/   "
      ]
  , unlines
      [ " /\\_/\\  "
      , "( ^.^ )  "
      , "  >☕<   "
      ]
  ]
