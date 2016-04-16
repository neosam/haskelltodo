{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Todo.Changelog (
                      -- * The Log type
                      Log (..),
                      logHead,

                      -- * Log entries
                      LogEntry (..),
                      entryHash,
                      entryVersion,
                      entryParent,
                      entryTime,
                      entryAction,
                      EntryHash (..),
                      EntryVersion (..),
                      ParentLogEntry (..),

                      -- * Log actions
                      EntryAction (..),
                      newLog,
                      mkLog,
                      addLogEntry,
                      addLogAction,
                      logToTextList,
                      validateEntryHistory,
                      validateLogHistory,
                      rehashEntryHistory,
                      rehashLogHistory,
                      rebuildTaskStat
                      ) where

import           Codec.Utils          (Octet)
import           Control.Lens
import           Control.Monad
import           Data.Binary.Put
import qualified Data.ByteString      as B
import qualified Data.ByteString.Lazy as BL
import qualified Data.Digest.SHA256   as Sha256
import           Data.Int
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as TE
import           Data.Time
import           Data.UUID            (UUID)
import qualified Data.UUID            as U
import qualified Data.UUID.V4         as UuidV4
import           Todo.Todo


{-| Store the logs and some meta data. -}
data Log = Log {
  -- | Holds the newest entry which logs back.
  _logHead :: ParentLogEntry
} deriving (Show, Read)


data EntryHash = EmptyHash
  | DblSha256Hash [Octet]
  deriving (Show, Read, Eq)

data EntryVersion = Version0_1_0
  deriving (Show, Read, Eq, Enum)

currentVersion :: EntryVersion
currentVersion = Version0_1_0

data EntryAction =
   -- | New active action is added to the list
     NewActiveAction ActiveTask

   -- | An active action is completed
   | CompletedActiveAction ActiveTask

   -- | A booled action was added
   | NewPooledAction PooledTask

   -- | New pooled tasks were activated
   | ActivationAction [ActiveTask]

   -- | Cleanup was performed on the list
   | CleanupAction [ActiveTask]
 deriving (Show, Read)

{-| One log entry which recursively holds the previous entry
    The log entry contains a hash value which combines the other
    attributes.  The hash should be the outcome of these values chained:

    - Version string
    - Parent hash
    - Systime
    - Action stringified
-}
data LogEntry = LogEntry {
  _entryHash    :: EntryHash,
  _entryVersion :: EntryVersion,
  _entryParent  :: ParentLogEntry,
  _entryTime    :: ZonedTime,
  _entryAction  :: EntryAction
} deriving (Show, Read)

data ParentLogEntry =
    PrevLogEntry LogEntry
  | StartLogEntry UUID
  | EmptyLogEntry
 deriving (Show, Read)


makeLenses ''LogEntry
makeLenses ''Log

enumToInt16 :: Enum a => a -> Int16
enumToInt16 enum = fromInteger $ toInteger $ fromEnum enum

startParentIdFlag :: Int8
startParentIdFlag = 0
prevParentIdFlag :: Int8
prevParentIdFlag = 1

putHash :: EntryHash -> Put
putHash (DblSha256Hash hash) = do
  putInt8 0
  forM_ hash $ \x -> putWord8 x

putParentUuid :: ParentLogEntry -> Put
putParentUuid (StartLogEntry uuid) = do
  putInt8 startParentIdFlag
  putLazyByteString $ U.toByteString uuid
putParentUuid (PrevLogEntry entry) = do
  putInt8 prevParentIdFlag
  putHash $ entry ^. entryHash

timeToInt32 :: ZonedTime -> Int32
timeToInt32 t =
  let tAsUnix = formatTime defaultTimeLocale "%s" utcT
      utcT = zonedTimeToUTC t
      unixTimeInt = read tAsUnix
      unixTimeInt32 = fromInteger $ toInteger unixTimeInt
   in unixTimeInt32

putTime :: ZonedTime -> Put
putTime t = do
  let tStr = show t
      tText = T.pack tStr
      tBs = TE.encodeUtf8 tText
  putInt8 $ fromInteger $ toInteger $ B.length tBs
  putByteString tBs

putLogEntry :: LogEntry -> Put
putLogEntry logEntry = do
  putInt16le $ enumToInt16 $ logEntry ^. entryVersion
  putParentUuid $ logEntry ^. entryParent
  putTime $ logEntry ^. entryTime
  putEntryAction $ logEntry ^. entryAction


putEntryAction :: EntryAction -> Put
putEntryAction (NewActiveAction aTask) = do
  putInt8 1
  putActiveTask aTask
putEntryAction (CompletedActiveAction aTask) = do
  putInt8 2
  putActiveTask aTask
putEntryAction (NewPooledAction pTask) = do
  putInt8 3
  putPooledTask pTask
putEntryAction (ActivationAction aTasks) = do
  putInt8 4
  putInt16le $ fromInteger $ toInteger $ length aTasks
  forM_ aTasks $ \aTask -> putActiveTask aTask
putEntryAction (CleanupAction aTasks) = do
  putInt8 5
  putInt16le $ fromInteger $ toInteger $ length aTasks
  forM_ aTasks $ \aTask -> putActiveTask aTask

logEntryToDblSha256 :: LogEntry -> EntryHash
logEntryToDblSha256 logEntry =
  let byteString = runPut (putLogEntry logEntry)
      octets = BL.unpack byteString
      sha256 = Sha256.hash octets
      dblSha256 = Sha256.hash sha256
   in DblSha256Hash dblSha256



-- | Create a complete new log
newLog :: IO Log
newLog = do
  uuid <- UuidV4.nextRandom
  return $ mkLog uuid

-- | Create a new log with the given UUID
mkLog :: UUID -> Log
mkLog uuid = Log $ StartLogEntry uuid

addLogEntry :: LogEntry -> Log -> Log
addLogEntry entry = over logHead $ \head ->
  let entry' = set entryParent head entry
      hash = logEntryToDblSha256 entry'
      entry'' = set entryHash hash entry'
   in PrevLogEntry entry''

addLogAction :: EntryAction -> ZonedTime -> Log -> Log
addLogAction action time =
  let logEntry = LogEntry EmptyHash currentVersion EmptyLogEntry time action
   in addLogEntry logEntry


logToList :: Log -> [LogEntry]
logToList log =
  parentEntryToList $ log ^. logHead

parentEntryToList :: ParentLogEntry -> [LogEntry]
parentEntryToList (StartLogEntry _) = []
parentEntryToList (PrevLogEntry entry) = entry : parentEntryToList prevEntry
  where prevEntry = entry ^. entryParent

logToTextList :: Log -> [Text]
logToTextList log =
  map logEntryToText $ parentEntryToList (log ^. logHead)

logEntryToText :: LogEntry -> Text
logEntryToText entry =
  (T.pack $ show $ entry^.entryTime)
    `T.append` ":  " `T.append` (entryActionToText (entry ^. entryAction))

activeTaskToText :: ActiveTask -> Text
activeTaskToText aTask =
  (aTask ^. atTask . tTitle) `T.append`
   ", due " `T.append` (T.pack $ show (aTask ^. atDue))

mergeTextNewln :: Text -> Text -> Text
mergeTextNewln t1 t2 = t1 `T.append` "\n" `T.append` t2

entryActionToText :: EntryAction -> Text
entryActionToText (NewActiveAction aTask) =
  "Add Task: " `T.append` (activeTaskToText aTask)
entryActionToText (NewPooledAction pTask) =
  "Add Pool: " `T.append` (pTask ^. ptTask . tTitle)
entryActionToText (CompletedActiveAction aTask) =
  "Completed Task:  " `T.append` (activeTaskToText aTask)
entryActionToText (ActivationAction aTasks) =
  "Many tasks activated: " `T.append`
    (foldl mergeTextNewln "" $ map (\x -> " - " `T.append` activeTaskToText x) aTasks)

validateEntryHistory :: Maybe EntryHash -> LogEntry -> (Bool, [Text])
validateEntryHistory untilHash logEntry =
  let verificationHash = logEntryToDblSha256 logEntry
      logText = logEntryToText logEntry
  in if (Just $ logEntry ^. entryHash) == untilHash
        then (True, [logText `T.append` " (checked)"])
        else if verificationHash == (logEntry ^. entryHash)
                then case logEntry ^. entryParent of
                       StartLogEntry _ -> (True, [logText `T.append` " (checked)"])
                       PrevLogEntry prevLogEntry ->
                          let (res, log)
                                = validateEntryHistory untilHash prevLogEntry
                              log' = (logText `T.append` " (checked)") : log
                           in (res, log')
                else (False, [logText `T.append` " (failed)"])


validateLogHistory :: Maybe EntryHash -> Log -> (Bool, [Text])
validateLogHistory untilHash log =
  case log ^. logHead of
    StartLogEntry _ -> (True, ["Emty log passed"])
    PrevLogEntry logEntry -> validateEntryHistory untilHash logEntry

rehashEntryHistory :: LogEntry -> LogEntry
rehashEntryHistory logEntry =
  let rehashedParent = case logEntry ^. entryParent of
                         StartLogEntry _ -> logEntry ^. entryParent
                         PrevLogEntry prevLogEntry ->
                           PrevLogEntry $ rehashEntryHistory prevLogEntry
      logEntry' = set entryParent rehashedParent logEntry
      newHash = logEntryToDblSha256 logEntry'
   in set entryHash newHash logEntry'

rehashLogHistory :: Log -> Log
rehashLogHistory log =
  case log ^. logHead of
    StartLogEntry _ -> log
    PrevLogEntry logEntry ->
      set logHead (PrevLogEntry $ rehashEntryHistory logEntry) log


{-| Applies a LogEntry to a TaskStat and returns the result. -}
statStep :: (LogEntry, TaskStat) -> TaskStat
statStep (logEntry, stat) = case logEntry ^. entryAction of
  NewActiveAction aTask -> addActiveTaskType aTask stat
  NewPooledAction pTask -> addPooledTaskType pTask stat
  CompletedActiveAction aTask -> markDone (aTask ^. atTask . tTitle) stat
  ActivationAction aTasks ->
    foldr addActiveTaskType stat aTasks

{-| Rebuild the whole TaskStat from a Log -}
rebuildTaskStat :: Log -> TaskStat
rebuildTaskStat log =
  let entries = logToEntryList log
   in rebuildTaskStatFromList entries

applyLogEntriesToTaskStat :: [LogEntry] -> TaskStat -> TaskStat
applyLogEntriesToTaskStat entries taskStat = foldr aux taskStat entries
    where aux entry stat = statStep (entry, stat)

rebuildTaskStatFromList :: [LogEntry] -> TaskStat
rebuildTaskStatFromList entries =
  applyLogEntriesToTaskStat entries emptyTaskStat

{-| Turns the Log to a list of its LogEntries.
    Starting with the most recent LogEntry. -}
logToEntryList :: Log -> [LogEntry]
logToEntryList log =
  let parentEntry = log ^. logHead
   in parentEntryToEntryList parentEntry

{-| Turns a LogEntry and its parent to a List.
    Starting with the most recent LogEntry. -}
parentEntryToEntryList :: ParentLogEntry -> [LogEntry]
parentEntryToEntryList (StartLogEntry _) = []
parentEntryToEntryList (PrevLogEntry e) =
  e : (parentEntryToEntryList $ e ^. entryParent)
