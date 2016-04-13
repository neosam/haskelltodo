{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

import Todo.Todo
import Todo.Changelog as CL
import Control.Lens
import Control.Monad.State.Lazy
import System.IO (hFlush, stdout, openFile, hGetContents, hClose,
                  IOMode(ReadMode))
import Control.Exception (SomeException, try)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Time (getZonedTime)

data Menu = Menu {
     _menuTitle :: T.Text,
     _exitLabel :: T.Text,
     _menuEntries :: [(T.Text, MenuEntry)]
}


data TuiStat = TuiStat {
  _tuiFilename :: T.Text,
  _tuiMainMenu :: Menu,
  _tuiTaskStat :: TaskStat,
  _tuiLog :: CL.Log
}

type TuiState = StateT TuiStat IO

data MenuEntry = SubMenu Menu
               | IOAction (TuiState ())

makeLenses ''Menu
makeLenses ''TuiStat


newline :: TuiState ()
newline = lift $ putStr "\n"

tPutStr :: T.Text -> TuiState ()
tPutStr str = lift $ TIO.putStr str


run :: TuiStat -> IO TuiStat
run stat = execStateT runTuiStat stat

runTuiStat :: TuiState ()
runTuiStat = do
  load
  loadLog
  mainMenu <- use tuiMainMenu
  doMenu mainMenu


update :: TuiState ()
update = do
  ts <- use tuiTaskStat
  ts' <- lift $ updateTaskStat ts
  tuiTaskStat .= ts'

doMenu :: Menu -> TuiState ()
doMenu menu = do
  update
  save
  newline
  ts <- use tuiTaskStat
  newline
  tPutStr $ menu ^. menuTitle
  newline
  printMenuEntries menu
  tPutStr "0 - "
  tPutStr $ menu ^. exitLabel
  newline
  back <- doMenuUserInput menu
  if back
     then return ()
     else doMenu menu

doMenuUserInput :: Menu -> TuiState Bool
doMenuUserInput menu = do
  choice <- promptInt "> "
  if choice == 0
     then return True
     else do
       let entries = menu ^. menuEntries
           (_, entry) = entries !! (choice - 1)
       doMenuEntry entry

promptString :: T.Text -> TuiState T.Text
promptString str = do
  lift $ TIO.putStr str
  lift $ hFlush stdout
  lift $ TIO.getLine

promptInt :: T.Text -> TuiState Int
promptInt str = do
   lift $ TIO.putStr str
   lift $ hFlush stdout
   eitherInt <- (lift $ try readLn) :: StateT TuiStat IO (Either SomeException Int)
   case eitherInt of
     Left _ -> do
       lift $ putStr "Try again\n"
       promptInt str
     Right i -> return i

promptFloat :: T.Text -> TuiState Float
promptFloat str = do
   lift $ TIO.putStr str
   lift $ hFlush stdout
   eitherFloat <- (lift $ try readLn) :: StateT TuiStat IO (Either SomeException Float)
   case eitherFloat of
     Left _ -> do
       lift $ putStr "Try again\n"
       promptFloat str
     Right f -> return f


doMenuEntry :: MenuEntry -> TuiState Bool
doMenuEntry (SubMenu menu) = do
  doMenu menu
  return False
doMenuEntry (IOAction state) = do
  state
  return False

printMenuEntries :: Menu -> TuiState ()
printMenuEntries menu = do
  let entries = menu ^. menuEntries
  let indices = [1..] :: [Int]
      zipped = zip indices entries
  forM_ zipped $ \(i, (title, _)) -> do
    tPutStr $ T.pack $ show i
    tPutStr " - "
    tPutStr title
    newline


mainMenu = Menu "Todo - Main" "Exit" [
  ("Add active task", IOAction addActiveTaskAction),
  ("Add pooled task", IOAction addScheduledTaskAction),
  ("Activate pooled tasks", IOAction activateAction),
  ("Show Tasks", SubMenu (Menu "Show Tasks" "Back" [
    ("Show active tasks", IOAction showActivesAction),
    ("Show overdue tasks", IOAction showOverduesAction),
    ("Show tasks next 2 days", IOAction (showTasksNextDaysAction 2)),
    ("Show tasks next 4 days", IOAction (showTasksNextDaysAction 4)),
    ("Show tasks next 7 days", IOAction (showTasksNextDaysAction 7)),
    ("Show tasks next 30 days", IOAction (showTasksNextDaysAction 30))
  ])),
  ("Mark done", IOAction markDoneAction),
  ("Verify log", IOAction verifyLogAction),
  ("Show log", IOAction showLogAction),
  ("Rehash log", IOAction rehashLogAction)
 ]

rehashLogAction :: TuiState ()
rehashLogAction = do
  tuiLog %= rehashLogHistory

verifyLogAction :: TuiState ()
verifyLogAction = do
  log <- use tuiLog
  let (result, logLines) = validateLogHistory Nothing log
  lift $ forM_ logLines $ \x -> do TIO.putStr x
                                   TIO.putStr "\n"
  if result
     then lift $ TIO.putStr "Log verification successful.\n"
     else lift $ TIO.putStr "Log verificatuon failed.\n"

showLogAction :: TuiState ()
showLogAction = do
  log <- use tuiLog
  let logLines = logToTextList log
      outText = T.unlines logLines
  lift $ TIO.putStr outText

markDoneAction :: TuiState ()
markDoneAction = do
  stat <- get
  let actives = stat ^.. allActives
      menuList = map activeToMarkDoneAction actives
      menu = Menu "Mark done" "Done" menuList
  doMenu menu

activeToMarkDoneAction :: ActiveTask -> (T.Text, MenuEntry)
activeToMarkDoneAction aTask =
  let title = (aTask ^. atTask . tTitle) :: T.Text
  in (title, IOAction $ markTaskDone aTask)

markTaskDone :: ActiveTask -> TuiState ()
markTaskDone aTask = do
  let title = aTask ^. atTask . tTitle
  tuiTaskStat %= markDone title
  time <- lift $ getZonedTime
  tuiLog %= CL.addLogAction (CompletedActiveAction aTask) time
  tuiTaskStat %= cleanup
  return ()


cleanupAction :: TuiState ()
cleanupAction = tuiTaskStat %= cleanup

allActives :: Traversal' TuiStat ActiveTask
allActives = tuiTaskStat.actives.traverse

allOverdues :: TuiStat -> Traversal' TuiStat ActiveTask
allOverdues stat = tuiTaskStat.(overdues $ stat ^. tuiTaskStat.today)

showActivesAction :: TuiState ()
showActivesAction = showActives allActives

filteredNextDays :: TuiStat -> Int -> Traversal' ActiveTask ActiveTask
filteredNextDays stat n = filtered $ \aTask ->
  let day = addDays n $ stat ^. tuiTaskStat . today
   in (aTask^.atDue) < day

activesNextDays :: TuiStat -> Int -> Traversal' TuiStat ActiveTask
activesNextDays stat n = allActives.(filteredNextDays stat n)

showTasksNextDaysAction :: Int -> TuiState ()
showTasksNextDaysAction n = do
  stat <- get
  showActives (activesNextDays stat n)

showOverduesAction :: TuiState ()
showOverduesAction = do
  stat <- get
  showActives (allOverdues stat)

showActives :: Traversal' TuiStat ActiveTask -> TuiState ()
showActives tr = do
  st <- get
  let aTasks = st ^.. tr
  mapM_ printActive $ st ^.. tr
  tPutStr "Task count:  "
  tPutStr $ T.pack $ show $ length aTasks
  tPutStr "\n"
  _ <- promptString "Press enter key to continue..."
  return ()

printActive :: ActiveTask -> TuiState ()
printActive aTask = do
  tPutStr $ T.pack $ show $ aTask ^. atDue
  tPutStr ":  "
  tPutStr $ aTask ^. atTask . tTitle
  tPutStr " ("
  tPutStr $ if aTask ^. atFinished == Nothing
               then "todo"
               else "done"
  tPutStr ")\n"

activateAction :: TuiState ()
activateAction = do
  taskStat <- use tuiTaskStat
  let (aTasks, stat) = activateR taskStat
  tuiTaskStat .= stat
  time <- lift $ getZonedTime
  tuiLog %= CL.addLogAction (ActivationAction aTasks) time

liftSt :: State a b -> StateT a IO b
liftSt st = do
  x <- get
  let (val, x') = runState st x
  put x'
  return val

addActiveTaskAction :: TuiState ()
addActiveTaskAction = do
  title <- promptString "Title: "
  desc <- promptString "Description: "
  factor <- promptFloat "Factor: "
  dueDays <- promptInt "Due in days: "
  taskStat <- use tuiTaskStat
  let (aTask, stat) = addActiveTaskR (title, desc, factor, dueDays) taskStat
  tuiTaskStat .= stat
  time <- lift $ getZonedTime
  tuiLog %= CL.addLogAction (NewActiveAction aTask) time


addScheduledTaskAction :: TuiState ()
addScheduledTaskAction = do
  title <- promptString "Title: "
  desc <- promptString "Description: "
  factor <- promptFloat "Factor: "
  dueDays <- promptInt "Due in days: "
  prop <- promptFloat "Propability to be picked: "
  coolDown <- promptInt "Cool down days: "
  taskStat <- use tuiTaskStat
  let (pTask, stat) = addPooledTaskR (title, desc, factor, dueDays, prop, coolDown) taskStat
  tuiTaskStat .= stat
  time <- lift $ getZonedTime
  tuiLog %= CL.addLogAction (NewPooledAction pTask) time

save :: TuiState ()
save = do
  ts <- use tuiTaskStat
  filename <- use tuiFilename
  let saveStr = show ts
  lift $ TIO.writeFile (T.unpack filename) $ T.pack saveStr

  log <- use tuiLog
  let logFilename = T.append filename ".log"
  let logSaveStr = show log
  lift $ TIO.writeFile (T.unpack logFilename) $ T.pack logSaveStr

loadLogUnsafe :: String -> IO CL.Log
loadLogUnsafe filename = do
  text <- TIO.readFile filename
  let log = (read (T.unpack text)) :: CL.Log
  return log

loadLog :: TuiState ()
loadLog = do
  filename' <- use tuiFilename
  let filename = T.append filename' ".log"
  let eitherLogIO = (try $ loadLogUnsafe (T.unpack filename)) :: IO (Either SomeException CL.Log)
  eitherLog <- lift $ eitherLogIO
  newLogVal <- lift $ newLog
  tuiLog .= case eitherLog of
    Left _ -> newLogVal
    Right log -> log

load :: TuiState ()
load = do
  filename <- use tuiFilename
  maybeTm <- lift $ loadTm $ T.unpack filename
  case maybeTm of
    Nothing -> return ()
    Just tm -> do
      tuiTaskStat .= tm

main :: IO ()
main = do
  log <- CL.newLog
  let tuiStat = TuiStat "state.sav" mainMenu emptyTaskStat log
  _ <- run tuiStat
  return ()

