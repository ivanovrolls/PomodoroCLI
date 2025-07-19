module Input (inputListener) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, hGetChar)
import Data.IORef
import Control.Monad (when, forever)
import System.IO (hFlush, stdout)
import System.Posix.Process (exitImmediately)
import System.Exit (ExitCode(..))  -- for ExitSuccess, ExitFailure, etc.
import System.Console.ANSI (cursorUpLine, clearLine)

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
                clearLine
                writeIORef skipFlag True
            'p' -> do
                paused <- readIORef pauseFlag
                let newPaused = not paused
                writeIORef pauseFlag newPaused
                if paused 
                then do
                    -- We just resumed, clear previous animation + timer output:
                    clearLines (catHeight)  -- catHeight from Animation.hs
                    putStrLn "\rResumed."
                else do
                    clearLine
                    putStrLn "\rPaused."
            _ -> return () --ignore other inputs
        
catHeight :: Int
catHeight = 2

clearLines :: Int -> IO ()
clearLines 0 = return ()
clearLines n = do
    clearLine
    cursorUpLine 1
    clearLines (n - 1)