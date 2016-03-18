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

data MenuEntry = SubMenu Menu
               | IOAction (StateT TaskStat IO ())

makeLenses ''Menu

newline :: IO ()
newline = putStr "\n"

doMenu :: TaskStat -> Menu -> IO TaskStat
doMenu ts menu = do
  newline
  putStr $ show ts
  newline
  putStr $ menu ^. menuTitle
  newline
  printMenuEntries $ menu ^. menuEntries
  putStr "0 - "
  putStr $ menu ^. exitLabel
  newline
  (back, ts') <- doMenuUserInput ts menu
  if back
     then return ts'
     else doMenu ts' menu

doMenuUserInput :: TaskStat -> Menu -> IO (Bool, TaskStat)
doMenuUserInput ts menu = do
  choice <- promptInt "> "
  if choice == 0
     then return (True, ts)
     else do
       let entries = menu ^. menuEntries
           (_, entry) = entries !! (choice - 1)
       doMenuEntry ts entry

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


doMenuEntry :: TaskStat -> MenuEntry -> IO (Bool, TaskStat)
doMenuEntry ts (SubMenu menu) = do
  ts' <- doMenu ts menu
  return (False, ts')
doMenuEntry ts (IOAction state) = do
  ts' <- execStateT state ts
  return (False, ts')

printMenuEntries :: [(String, MenuEntry)] -> IO ()
printMenuEntries entries = do
  let indices = [1..] :: [Int]
      zipped = zip indices entries
  mapM (\(i, (title, _)) -> do
    putStr $ show i
    putStr " - "
    putStr title
    newline
   ) zipped
  return ()

menu = Menu "Todo - Main" "Exit" [
  ("Add active task", IOAction addActiveTaskAction)
 ]

liftSt :: State a b -> StateT a IO b
liftSt st = do
  x <- get
  let (val, x') = runState st x
  put x'
  return val

addActiveTaskAction :: StateT TaskStat IO ()
addActiveTaskAction = do
  title <- lift $ promptString "Title: "
  desc <- lift $ promptString "Description: "
  factor <- lift $ promptFloat "Factor: "
  dueDays <- lift $ promptInt "Due in days: "
  liftSt $ addActiveTask (title, desc, factor, dueDays)

main :: IO ()
main = do
  doMenu emptyTaskStat menu
  return ()

