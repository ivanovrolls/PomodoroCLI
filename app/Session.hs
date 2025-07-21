module Session (runSession) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar, newEmptyMVar)
import Control.Concurrent.MVar (MVar, newEmptyMVar, putMVar, tryTakeMVar, takeMVar)
import Data.IORef (IORef, newIORef)
import Animation (animate)
import Timer (timer)
import Input (inputListener)
import Data.IORef (readIORef, writeIORef)

runSession :: Int -> [String] -> String -> IORef Bool -> MVar Bool -> IO Bool
runSession duration frames finishMsg pauseFlag skipSignal = do
    skippedRef <- newIORef False
    doneAnim <- newEmptyMVar
    doneTimer <- newEmptyMVar

    _ <- forkIO $ animate frames (duration * 2) pauseFlag skipSignal skippedRef >> putMVar doneAnim ()
    _ <- forkIO $ timer duration pauseFlag skipSignal skippedRef >> putMVar doneTimer ()

    takeMVar doneAnim
    takeMVar doneTimer
    putStrLn finishMsg

    readIORef skippedRef

