module Main where

import GHC.IO.Handle.FD
import System.IO
import Control.Concurrent
import System.Environment
import System.Posix.Files (createNamedPipe, accessModes)
import System.Process (createProcess, proc)

handleMsg s =
  do putStr "msg: "
     putStrLn s

readMsg h =
  do done <- hIsEOF h
     if done
     then return ()
     else 
       do c <- hGetLine h
          handleMsg c

readLoop h =
  do readMsg h
     readLoop h
     return ()

data Pipe = Pipe String Handle
data Axon = Axon String String String
data AxonPipe = AxonPipe String Pipe Pipe

axonPipe :: Axon -> IO AxonPipe
axonPipe (Axon n r w) =
  do createNamedPipe r accessModes
     createNamedPipe w accessModes
     -- Waits for fifos to have other managers before proceeding
     wP <- openFileBlocking w ReadMode
     rP <- openFileBlocking r WriteMode
     return $ AxonPipe n (Pipe r rP) (Pipe w wP)

makeAxon n =
  do let r = n ++ "-r"
     let w = n ++ "-w"
     return $ Axon n r w

connectAxons axons =
  do axonPipes <- mapM axonPipe axons
     mapM (\(AxonPipe _ _ (Pipe _ w)) -> readLoop w) axonPipes
     return ()

spinUpAxons axons =
  do mapM (\(Axon name r w) ->
       createProcess (proc name [r, w])) axons
     return ()

main =
  do names <- getArgs
     axons <- mapM makeAxon names
     ran <- newEmptyMVar
     forkIO $ connectAxons axons >> putMVar ran ()
     forkIO $ spinUpAxons axons
     takeMVar ran

