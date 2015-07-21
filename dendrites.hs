module Main where

import GHC.IO.Handle.FD
import System.IO
import Control.Concurrent
import System.Environment
import System.Posix.Files (createNamedPipe, accessModes)
import System.Process (createProcess, proc)
import Data.Text (splitOn, pack, unpack)
import Control.Exception (catch)
import Control.Monad (unless)

data Pipe = Pipe String Handle
data Axon = Axon String String String deriving Show
data AxonPipe = AxonPipe String Pipe Pipe

readMsg name pipe pipes =
  do let (AxonPipe from _ (Pipe _ h)) = pipe
     done <- hIsEOF h
     unless done $
       do msg <- hGetLine h
          unless (msg == "") $
            let (a:m:_) = splitMsg msg 
            in sendMsg m from (getAxonPipe a pipes)

-- Send a message from one AxonPipe to another
sendMsg msg from (AxonPipe to (Pipe _ r) _) =
  do hPutStrLn r (from ++ " " ++ msg)
     hFlush r
     return ()

splitMsg s = map unpack $ splitOn (pack " ") (pack s)

-- Lookup an AxonPipe by name
getAxonPipe name (p@(AxonPipe n _ _):axa)
  | name == n = p
  | otherwise = getAxonPipe name axa

-- Precalculate the current pipe
messageLoop' name pipe pipes =
  do looped <- newEmptyMVar
     forkFinally (do
       readMsg name pipe pipes
       child <- messageLoop' name pipe pipes
       takeMVar child
       return ()) (\_ -> putMVar looped ())
     return looped

-- Read and redirect messages from a given pipe
messageLoop :: String -> [AxonPipe] -> IO (MVar ())
messageLoop name pipes = messageLoop' name (getAxonPipe name pipes) pipes

-- Constructs an AxonPipe for interacting with an Axon
axonPipe :: Axon -> IO AxonPipe
axonPipe (Axon n r w) =
  do rPM <- newEmptyMVar
     wPM <- newEmptyMVar
     forkIO $ openFileBlocking w ReadMode >>= putMVar wPM
     forkIO $ openFileBlocking r WriteMode >>= putMVar rPM
     -- Wait for each fifo to have other managers before proceeding
     rP <- takeMVar rPM
     wP <- takeMVar wPM
     return $ AxonPipe n (Pipe r rP) (Pipe w wP)

-- Make the fifos for an axon
makeAxon n =
  do let r = n ++ "-r"
     let w = n ++ "-w"
     catchAny (createNamedPipe r accessModes) (const $ return ())
     catchAny (createNamedPipe w accessModes) (const $ return ())
     return $ Axon n r w

catchAny :: IO a -> (IOError -> IO a) -> IO a
catchAny = catch

-- Start a message loop for each axon
connectAxa :: [Axon] -> IO ()
connectAxa axa =
  do axonPipes <- mapM axonPipe axa
     threads <- mapM (\(AxonPipe n _ _) -> messageLoop n axonPipes) axonPipes
     mapM_ takeMVar threads

-- Run axon processes
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

