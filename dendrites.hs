module Main where

import GHC.IO.Handle.FD
import System.IO
import Control.Concurrent
import System.Environment
import System.Posix.Files (createNamedPipe, accessModes)
import System.Process (createProcess, proc)
import Data.Text (splitOn, pack, unpack)
import Control.Exception (catch)

catchAny :: IO a -> (IOError -> IO a) -> IO a
catchAny = catch

splitMsg s = map unpack $ splitOn (pack " ") (pack s)

sendMsg msg from (AxonPipe to (Pipe _ r) _) =
  do hPutStrLn r (from ++ " " ++ msg)
     hFlush r
     return ()

readMsg name pipe pipes =
  do let (AxonPipe from _ (Pipe _ h)) = pipe
     done <- hIsEOF h
     if done
     then return ()
     else 
       do msg <- hGetLine h
          if msg == ""
          then return ()
          else
            let (a:m:_) = splitMsg msg 
            in sendMsg m from (getAxonPipe a pipes)

getAxonPipe name (p@(AxonPipe n _ _):axa) =
  if name == n
  then p
  else getAxonPipe name axa

pipeReadLoop name pipe pipes =
  do looped <- newEmptyMVar
     forkFinally (do
       readMsg name pipe pipes
       child <- pipeReadLoop name pipe pipes
       takeMVar child
       return ()) (\_ -> putMVar looped ())
     return looped
       

readLoop :: String -> [AxonPipe] -> IO (MVar ())
readLoop name pipes = pipeReadLoop name (getAxonPipe name pipes) pipes

data Pipe = Pipe String Handle
data Axon = Axon String String String deriving Show
data AxonPipe = AxonPipe String Pipe Pipe

axonPipe :: Axon -> IO AxonPipe
axonPipe (Axon n r w) =
  do -- Waits for fifos to have other managers before proceeding
     rPM <- newEmptyMVar
     wPM <- newEmptyMVar
     forkIO $ openFileBlocking w ReadMode >>= putMVar wPM
     forkIO $ openFileBlocking r WriteMode >>= putMVar rPM
     rP <- takeMVar rPM
     wP <- takeMVar wPM
     return $ AxonPipe n (Pipe r rP) (Pipe w wP)

makeAxon n =
  do let r = n ++ "-r"
     let w = n ++ "-w"
     catchAny (createNamedPipe r accessModes) (const $ return ())
     catchAny (createNamedPipe w accessModes) (const $ return ())
     return $ Axon n r w

connectAxa :: [Axon] -> IO ()
connectAxa axa =
  do axonPipes <- mapM axonPipe axa
     threads <- mapM (\(AxonPipe n _ _) -> readLoop n axonPipes) axonPipes
     mapM_ takeMVar threads

spinUpAxa axa =
  do mapM (\(Axon name r w) ->
       createProcess (proc name [r, w])) axa
     return ()

main =
  do names <- getArgs
     axa <- mapM makeAxon names
     ran <- newEmptyMVar
     forkIO $ connectAxa axa >> putMVar ran ()
     forkIO $ spinUpAxa axa
     takeMVar ran

