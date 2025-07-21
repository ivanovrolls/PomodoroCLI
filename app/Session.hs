module Session (runSession) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef)
import Animation (animate)
import Timer (timer)
import Input (inputListener)
import Data.IORef (readIORef)

runSession :: Int -> [String] -> String -> IO Bool
runSession duration frames finishMsg = do
    pauseFlag <- newIORef False
    skipFlag <- newIORef False
    _ <- forkIO (inputListener pauseFlag skipFlag)
    doneAnim <- newEmptyMVar
    doneTimer <- newEmptyMVar

    _ <- forkIO $ animate frames (duration * 2) pauseFlag skipFlag >> putMVar doneAnim ()
    _ <- forkIO $ timer duration pauseFlag skipFlag >> putMVar doneTimer ()

    takeMVar doneAnim
    takeMVar doneTimer
    putStrLn finishMsg

    skip <- readIORef skipFlag
    return skip
