module System.LogFS.Internal (
 runLogFS,
 Packet(..)
) where

import qualified Data.ByteString.Char8 as B
 (ByteString, pack, length, empty)

import qualified Control.Exception as E
 (Exception)

import System.Posix.Files
 (ownerWriteMode, ownerReadMode, ownerExecuteMode, groupWriteMode, groupReadMode, groupExecuteMode, otherWriteMode, otherReadMode, otherExecuteMode)

import System.Posix.Types
 (FileOffset, ByteCount, FileMode)

import Foreign.C.Error
 (Errno)

import System.Posix.IO
 (OpenFileFlags)

import System.Fuse
 (FileStat(..), EntryType(..), FuseContext(..), FuseOperations(..), FileSystemStats(..), SyncType, OpenMode, defaultFuseOps, fuseCtxUserID, fuseCtxGroupID, fuseRun, unionFileModes, eOK, getFuseContext, defaultFuseOps)

data Packet = Packet {
 _path :: FilePath,
 _payload :: B.ByteString
} deriving (Show)

type HT = ()


logString :: B.ByteString
logString = B.pack ""

runLogFS :: E.Exception e => String -> [String] -> (Packet -> IO ()) -> (String -> Bool) -> (e -> IO Errno) -> IO ()
runLogFS prog argv f dirFilter handler = do
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
     logGetFileSystemStats _ = do
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
     logGetFileStat dir = do
      ctx <- getFuseContext
      case (dirFilter dir) of
       True -> return $ Right $ dirStat ctx
       _ -> return $ Right $ fileStat ctx

     logAccess :: FilePath -> Int -> IO Errno
     logAccess _ _ = return eOK

     logCreateDirectory :: FilePath -> FileMode -> IO Errno
     logCreateDirectory _ _ = return eOK

     logOpenDirectory :: FilePath -> IO Errno
     logOpenDirectory _ = return eOK

     logReadDirectory :: FilePath -> IO (Either Errno [(FilePath, FileStat)])
     logReadDirectory _ = do
      ctx <- getFuseContext
      return $ Right
       [(".", dirStat ctx)
       ,("..", dirStat ctx)
       ]

     logSetFileSize :: FilePath -> FileOffset -> IO Errno
     logSetFileSize _ _ = return eOK

     logOpen :: FilePath -> OpenMode -> OpenFileFlags -> IO (Either Errno HT)
     logOpen _ _ _ = return (Right ())

     logRead :: FilePath -> HT -> ByteCount -> FileOffset -> IO (Either Errno B.ByteString)
     logRead _ _ _ _ = return $ Right $ B.empty

     logWrite :: FilePath -> HT -> B.ByteString -> FileOffset -> IO (Either Errno ByteCount)
     logWrite path _ byteString _ = do
      f $ Packet { _path = path, _payload = byteString }
      return $ Right $ fromIntegral $ B.length byteString

     logFlush :: FilePath -> HT -> IO Errno
     logFlush _ _ = return eOK

     logRelease :: FilePath -> HT -> IO ()
     logRelease _ _ = return ()

     logSynchronizeFile :: FilePath -> SyncType -> IO Errno
     logSynchronizeFile _ _ = return eOK

 fuseRun prog argv logFSOps (\e -> print e >> handler e)



dirStat :: FuseContext -> FileStat
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


fileStat :: FuseContext -> FileStat
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
