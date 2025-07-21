module Input (inputListener) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, hGetChar)
import Data.IORef
import Control.Monad (when, forever)
import System.IO (hFlush, stdout)
import System.Posix.Process (exitImmediately)
import System.Exit (ExitCode(..))  -- for ExitSuccess, ExitFailure, etc.
import System.Console.ANSI (cursorUpLine, clearLine, cursorDownLine)

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
                putStrLn "DEBUG: Skip flag set to True"
            'p' -> do
                paused <- readIORef pauseFlag
                let newPaused = not paused
                writeIORef pauseFlag newPaused
                putStrLn $ "DEBUG: Pause flag toggled to " ++ show newPaused
            _ -> return ()
        
catHeight :: Int
catHeight = 2

clearLines :: Int -> IO ()
clearLines 0 = return ()
clearLines n = do
    clearLine
    if n > 1 then cursorDownLine 1 else return ()
    clearLines (n - 1)
