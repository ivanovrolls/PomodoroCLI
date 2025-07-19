module Input (inputListener) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, hGetChar)
import Data.IORef
import Control.Monad (when, forever)
import System.IO (hFlush, stdout)

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
