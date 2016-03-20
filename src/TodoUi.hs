{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}

import Todo.Todo
import Control.Lens
import Control.Monad.State.Lazy
import System.IO (hFlush, stdout, openFile, hGetContents, hClose,
                  IOMode(ReadMode))
import Control.Exception (SomeException, try)

data Menu = Menu {
     _menuTitle :: String,
     _exitLabel :: String,
     _menuEntries :: [(String, MenuEntry)]
}


data TuiStat = TuiStat {
  _tuiFilename :: String,
  _tuiMainMenu :: Menu,
  _tuiTaskStat :: TaskStat
}

type TuiState = StateT TuiStat IO

data MenuEntry = SubMenu Menu
               | IOAction (TuiState ())

makeLenses ''Menu
makeLenses ''TuiStat


newline :: TuiState ()
newline = lift $ putStr "\n"

tPutStr :: String -> TuiState ()
tPutStr str = lift $ putStr str


run :: TuiStat -> IO TuiStat
run stat = execStateT runTuiStat stat

runTuiStat :: TuiState ()
runTuiStat = do
  load
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
  tPutStr $ show ts
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

promptString :: String -> TuiState String
promptString str = do
  lift $ putStr str
  lift $ hFlush stdout
  lift $ getLine

promptInt :: String -> TuiState Int
promptInt str = do
   lift $ putStr str
   lift $ hFlush stdout
   eitherInt <- (lift $ try readLn) :: StateT TuiStat IO (Either SomeException Int)
   case eitherInt of
     Left _ -> do
       lift $ putStr "Try again\n"
       promptInt str
     Right i -> return i

promptFloat :: String -> TuiState Float
promptFloat str = do
   lift $ putStr str
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
  mapM (\(i, (title, _)) -> do
    tPutStr $ show i
    tPutStr " - "
    tPutStr title
    newline
   ) zipped
  return ()

mainMenu = Menu "Todo - Main" "Exit" [
  ("Add active task", IOAction addActiveTaskAction),
  ("Add pooled task", IOAction addScheduledTaskAction),
  ("Activate pooled tasks", IOAction activateAction)
 ]

activateAction :: TuiState ()
activateAction = tuiTaskStat %= activate

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
  tuiTaskStat %= addActiveTask (title, desc, factor, dueDays)

addScheduledTaskAction :: TuiState ()
addScheduledTaskAction = do
  title <- promptString "Title: "
  desc <- promptString "Description: "
  factor <- promptFloat "Factor: "
  dueDays <- promptInt "Due in days:"
  prop <- promptFloat "Propability to be picked: "
  tuiTaskStat %= addPooledTask (title, desc, factor, dueDays, prop)

save :: TuiState ()
save = do
  ts <- use tuiTaskStat
  filename <- use tuiFilename
  let saveStr = show ts
  lift $ writeFile filename saveStr

load :: TuiState ()
load = do
  filename <- use tuiFilename
  maybeTm <- lift $ loadTm filename
  case maybeTm of
    Nothing -> return ()
    Just tm -> do
      tuiTaskStat .= tm

main :: IO ()
main = do
  let tuiStat = TuiStat "state.sav" mainMenu emptyTaskStat
  _ <- run tuiStat
  return ()

