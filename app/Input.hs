module Input (inputListener) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, hGetChar)
import Data.IORef
import Control.Monad (when, forever)
import System.IO (hFlush, stdout)

--listen for input
inputListener :: IORef Bool -> IORef Bool -> IORef Bool -> IO ()
inputListener pauseFlag skipFlag quitFlag = do
    hSetBuffering stdin NoBuffering --set input so u dont have to press enter
    forever $ do
        c <- hGetChar stdin
        case c of
            'q' -> do
                putStrLn "\nQuitting..."
                writeIORef quitFlag True
            's' -> do
                writeIORef skipFlag True
            'p' -> do
                paused <- readIORef pauseFlag
                writeIORef pauseFlag (not paused) --toggle pause state
                putStrLn $ if paused then "\nResumed." else "\nPaused."
            _ -> return () --ignore other inputs
        