module Main where

import GHC.IO.Handle.FD
import System.IO
import Control.Concurrent
import System.Posix.Files (createNamedPipe)

handleMsg s =
  do putStr "msg: "
     putStrLn s

readMsg =
  do h <- openFileBlocking "fifo" ReadMode
     done <- hIsEOF h
     if done
     then return ()
     else 
       do c <- hGetLine h
          handleMsg c

readLoop =
  do readMsg
     readLoop

main =
  do loopDone <- newEmptyMVar
     forkIO $ readLoop >> putMVar loopDone ()
     putStrLn "ready"
     takeMVar loopDone
     

