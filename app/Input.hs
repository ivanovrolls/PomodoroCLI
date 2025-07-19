module Input (inputListener) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, hGetChar)
import Data.IORef
import Control.Monad (when, forever)
import System.IO (hFlush, stdout)
import System.Posix.Process (exitImmediately)
import System.Exit (ExitCode(..))  -- for ExitSuccess, ExitFailure, etc.


--listen for input
inputListener :: IORef Bool -> IORef Bool -> IO ()
inputListener pauseFlag skipFlag = do
    hSetBuffering stdin NoBuffering --set input so u dont have to press enter
    forever $ do
        c <- hGetChar stdin
        case c of
            'q' -> do
                putStrLn "\nQuitting..."
                exitImmediately ExitSuccess
            's' -> do
                writeIORef skipFlag True
            'p' -> do
                paused <- readIORef pauseFlag
                writeIORef pauseFlag (not paused) --toggle pause state
                putStrLn $ if paused then "\nResumed." else "\nPaused."
            _ -> return () --ignore other inputs
        