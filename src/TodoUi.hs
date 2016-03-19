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

doMenu :: Menu -> TuiState ()
doMenu menu = do
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
  choice <- lift $ promptInt "> "
  if choice == 0
     then return True
     else do
       let entries = menu ^. menuEntries
           (_, entry) = entries !! (choice - 1)
       doMenuEntry entry

promptString :: String -> IO String
promptString str = do
  putStr str
  hFlush stdout
  getLine

promptInt :: String -> IO Int
promptInt str = do
   putStr str
   hFlush stdout
   readLn

promptFloat :: String -> IO Float
promptFloat str = do
   putStr str
   hFlush stdout
   readLn


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
  ("Add active task", IOAction addActiveTaskAction)
 ]

liftSt :: State a b -> StateT a IO b
liftSt st = do
  x <- get
  let (val, x') = runState st x
  put x'
  return val

addActiveTaskAction :: TuiState ()
addActiveTaskAction = do
  title <- lift $ promptString "Title: "
  desc <- lift $ promptString "Description: "
  factor <- lift $ promptFloat "Factor: "
  dueDays <- lift $ promptInt "Due in days: "
  tuiTaskStat %= addActiveTask (title, desc, factor, dueDays)

main :: IO ()
main = do
  let tuiStat = TuiStat "state.sav" mainMenu emptyTaskStat
  _ <- run tuiStat
  return ()

