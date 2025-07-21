module Input (inputListener) where

import System.IO (hSetBuffering, BufferMode(NoBuffering), stdin, hGetChar)
import Control.Monad (forever)
import System.IO (hFlush, stdout)
import System.Posix.Process (exitImmediately)
import System.Exit (ExitCode(..))
import System.Console.ANSI (cursorUpLine, clearLine, cursorDownLine)
import Control.Concurrent.MVar (tryPutMVar, MVar)
import Data.IORef (IORef, readIORef, writeIORef)


inputListener :: IORef Bool -> MVar Bool -> IO ()
inputListener pauseFlag skipSignal = do
    hSetBuffering stdin NoBuffering
    forever $ do
        c <- hGetChar stdin
        case c of
            'q' -> do
                putStrLn "\nQuitting..."
                exitImmediately ExitSuccess
            's' -> do
                _ <- tryPutMVar skipSignal True
                putStrLn "DEBUG: Skip signal set"
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
