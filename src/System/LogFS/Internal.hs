module System.LogFS.Internal (
 runLogFS
) where

import qualified Data.ByteString.Char8 as B
import Control.Concurrent (forkIO)
import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, putMVar)
import Control.Monad (forever)
import qualified Control.Exception as E (Exception)
import System.Posix.Files
import System.Posix.Types
import Foreign.C.Error
import System.Posix.IO

import System.Fuse


data Packet = Packet {
 _path :: FilePath,
 _payload :: B.ByteString
} deriving (Show)

type FileName = String

type HT = ()

logString :: B.ByteString
logString = B.pack "log!\n"

logPath :: FilePath
logPath = "/log"

runLogFS :: E.Exception e => String -> [String] -> (String -> IO ()) -> (e -> IO Errno) -> IO ()
runLogFS prog argv f handler = do
 let
     logFSOps :: FuseOperations HT
     logFSOps =
      defaultFuseOps {
       fuseGetFileSystemStats = logGetFileSystemStats,
       fuseGetFileStat        = logGetFileStat,
       fuseAccess             = logAccess,
       fuseOpen               = logOpen,
       fuseRead               = logRead,
       fuseWrite              = logWrite,
       fuseFlush              = logFlush,
       fuseRelease            = logRelease,
       fuseSynchronizeFile    = logSynchronizeFile,
       fuseCreateDirectory    = logCreateDirectory,
       fuseOpenDirectory      = logOpenDirectory,
       fuseReadDirectory      = logReadDirectory,
       fuseSetFileSize        = logSetFileSize
     }

     logGetFileSystemStats :: String -> IO (Either Errno FileSystemStats)
     logGetFileSystemStats str = do
      f "GetFileSystemStats"
      return $ Right $ FileSystemStats
       { fsStatBlockSize       = 512
       , fsStatBlockCount      = 1
       , fsStatBlocksFree      = 1
       , fsStatBlocksAvailable = 1
       , fsStatFileCount       = 5
       , fsStatFilesFree       = 10
       , fsStatMaxNameLength   = 255
      }

     logGetFileStat :: FilePath -> IO (Either Errno FileStat)
     logGetFileStat "/" = do
      f "GetFileStat(1)"
      ctx <- getFuseContext
      return $ Right $ dirStat ctx
     logGetFileStat _ = do
      f "GetFileStat(2)"
      ctx <- getFuseContext
      return $ Right $ fileStat ctx

     logAccess :: FilePath -> Int -> IO Errno
     logAccess _ _ = do
      f "Access"
      return eOK

     logCreateDirectory :: FilePath -> FileMode -> IO Errno
     logCreateDirectory _ _ = f "CreateDirectory" >> return eOK

     logOpenDirectory :: FilePath -> IO Errno
     logOpenDirectory _ = f "OpenDirectory" >> return eOK

     logReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
     logReadDirectory _ = do
      f "ReadDirectory"
      ctx <- getFuseContext
      return $ Right
       [(".", dirStat ctx)
       ,("..", dirStat ctx)
       ,(logName, fileStat ctx)
       ]
      where (_:logName) = logPath

     logSetFileSize :: FilePath -> FileOffset -> IO Errno
     logSetFileSize _ _ = f "SetFileSize" >> return eOK

     logOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
     logOpen path mode flags = f "Open" >> return (Right ())

     logRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
     logRead path _ byteCount offset = do
      f "Read"
      return $ Right $ B.take (fromIntegral byteCount) $ B.drop (fromIntegral offset) logString

     logWrite :: FilePath -> HT -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
     logWrite path _ byteString offset = do
      f "Write"
      putStrLn $ show byteString
      return $ Right $ fromIntegral $ B.length byteString

     logFlush :: FilePath -> HT -> IO Errno
     logFlush _ _ = f "Flush" >> return eOK

     logRelease :: FilePath -> HT -> IO ()
     logRelease _ _ = f "Release" >> return ()

     logSynchronizeFile :: FilePath -> SyncType -> IO Errno
     logSynchronizeFile _ _ = f "SynchronizeFile" >> return eOK

 fuseRun prog argv logFSOps (\e -> print e >> handler e)



dirStat ctx = FileStat {
 statEntryType = Directory
 , statFileMode = foldr1 unionFileModes
   [ ownerReadMode
   , ownerExecuteMode
   , groupReadMode
   , groupExecuteMode
   , otherReadMode
   , otherExecuteMode
   , ownerWriteMode
   , groupWriteMode
   , otherWriteMode
   ]
 , statLinkCount        = 2
 , statFileOwner        = fuseCtxUserID ctx
 , statFileGroup        = fuseCtxGroupID ctx
 , statSpecialDeviceID  = 0
 , statFileSize         = 4096
 , statBlocks           = 1
 , statAccessTime       = 0
 , statModificationTime = 0
 , statStatusChangeTime = 0
 }


fileStat ctx = FileStat
 { statEntryType = RegularFile
 , statFileMode = foldr1 unionFileModes
    [ ownerReadMode
    , groupReadMode
    , otherReadMode
    , ownerWriteMode
    , groupWriteMode
    , otherWriteMode
    ]
 , statLinkCount        = 1
 , statFileOwner        = fuseCtxUserID ctx
 , statFileGroup        = fuseCtxGroupID ctx
 , statSpecialDeviceID  = 0
 , statFileSize         = fromIntegral $ B.length logString
 , statBlocks           = 1
 , statAccessTime       = 0
 , statModificationTime = 0
 , statStatusChangeTime = 0
 }
