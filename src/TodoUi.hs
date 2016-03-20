{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

import Todo.Todo
import Control.Lens
import Control.Monad.State.Lazy
import System.IO (hFlush, stdout)

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
run stat = execStateT (doMenu $ stat ^. tuiMainMenu) stat

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
   lift $ readLn

promptFloat :: String -> TuiState Float
promptFloat str = do
   lift $ putStr str
   lift $ hFlush stdout
   lift $ readLn


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
  ("Add scheduled task", IOAction addScheduledTaskAction)
 ]

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

main :: IO ()
main = do
  let tuiStat = TuiStat "state.sav" mainMenu emptyTaskStat
  _ <- run tuiStat
  return ()

