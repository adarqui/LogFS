module System.LogFS.Run (
 runBackend
) where

import System.DevUtils.Parser (runCmd)
import System.DevUtils.Cmd (Cmd(..))

import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, takeMVar, newEmptyMVar)
import Control.Concurrent.Chan (Chan, newChan, readChan, dupChan)
import Control.Monad (forever)

data Channel = Channel {
 _data :: String
} deriving (Show, Read)

p :: MVar String -> IO ()
p mv = do
 s <- takeMVar mv
 putStrLn $ "mv: " ++ s

p2 :: String -> Chan String -> IO ()
p2 id ch = do
 s <- readChan ch
 putStrLn $ "id: " ++ show id ++ ", ch: " ++ s

runBackend :: [String] -> IO (Chan String)
runBackend lfsArgv = do
 ch <- newChan
 mapM_ (\s -> runBackend' ch (runCmd s)) lfsArgv
 return ch

runBackend' :: Chan String -> Either String Cmd -> IO ()
runBackend' ch (Right (UrlRedis r)) = do dupChan ch >>= \nh -> (forkIO $ forever $ p2 "Redis" nh) >> return ()
runBackend' ch (Right (UrlZMQ z)) = do dupChan ch >>= \nh -> (forkIO $ forever $ p2 "ZMQ" nh) >> return ()
runBackend' ch (Right (UrlFile f)) = do dupChan ch >>= \nh -> (forkIO $ forever $ p2 "File" nh) >> return ()
runBackend' ch _ = putStrLn "Wrecked!"
