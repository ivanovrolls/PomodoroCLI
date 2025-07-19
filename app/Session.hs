module Session (runSession) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar)
import Data.IORef (IORef, newIORef)
import Animation (animate)
import Timer (timer)
import Input (inputListener)

runSession :: Int -> [String] -> String -> IO ()
runSession duration frames finishMsg = do
    pauseRef <- newIORef False
    _ <- forkIO $ inputListener pauseRef --start input listener in a separate thread
    doneAnim <- newEmptyMVar
    doneTimer <- newEmptyMVar

    _ <- forkIO $ animate frames (duration * 2) pauseRef >> putMVar doneAnim ()
    _ <- forkIO $ timer duration pauseRef >> putMVar doneTimer ()

    takeMVar doneAnim
    takeMVar doneTimer
    putStrLn finishMsg