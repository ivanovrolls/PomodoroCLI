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
import Control.Concurrent.MVar (tryTakeMVar, MVar)

catHeight :: Int
catHeight = 3

cursorUpN :: Int -> IO ()
cursorUpN n = cursorUpLine n

animate :: [String] -> Int -> IORef Bool -> MVar Bool -> IORef Bool -> IO ()
animate _ 0 _ _ _ = return ()
animate frames n pauseFlag skipSignal skippedRef = do
    skipTaken <- tryTakeMVar skipSignal
    case skipTaken of
        Just _ -> writeIORef skippedRef True
        Nothing -> return ()

    skipped <- readIORef skippedRef
    if skipped then do
        cursorUpN catHeight
        sequence_ [clearLine >> cursorDownLine 1 | _ <- [1..catHeight]]
        cursorUpN catHeight
        return ()
    else do
        paused <- readIORef pauseFlag
        if paused then do
            threadDelay 500000
            animate frames n pauseFlag skipSignal skippedRef
        else do
            cursorUpN catHeight
            sequence_ [clearLine >> cursorDownLine 1 | _ <- [1..catHeight]]
            cursorUpN catHeight
            putStr (frames !! (n `mod` length frames))
            hFlush stdout
            threadDelay 500000
            animate frames (n - 1) pauseFlag skipSignal skippedRef

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
