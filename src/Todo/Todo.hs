{-|
Module      : Todo.Todo
Description : Core todo module.
Copyright   : (c) Simon Goller, 2016
License     : BSD

Provides the basic functionality for the tasks.

-}

{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE BangPatterns #-}


module Todo.Todo (
  -- * Types
  TaskStat,
  ActiveTask,
  PooledTask,
  Task,

  -- * TaskStat State monad functions
  emptyTaskStat,
  addActiveTaskM,
  addActiveTask,
  addPooledTask,
  addPooledTaskM,
  activate,
  activateM,
  getOverdues,
  getOverduesM,
  cleanup,
  cleanupM,
  markDone,
  markDoneM,

  updateTaskStat,
  loadTmUnsafe,
  loadTm,

  -- * Lenses and stuff
  -- ** General access
  -- *** State
  actives,
  pool,
  today,
  rand,


  -- *** ActiveTask
  atTask,
  atDue,
  atFinished,

  -- *** PooledTask
  ptTask,
  ptDueDays,
  ptLastFinished,
  ptProp,

  -- *** Task
  tTitle,
  tDesc,
  tFactor,

  -- ** Filtered traversals
  unfinished,
  overdues,
  tasksToActivate,
  notActivePTasks,
  notCoolingDown
) where

import System.Random (StdGen, random, mkStdGen, newStdGen)
import Data.Time (Day, getCurrentTime, utctDay)
import qualified Data.Time as Time
import Control.Lens
import Control.Monad.State.Lazy
import System.IO (hFlush, stdout, openFile, hGetContents, hClose,
                  IOMode(ReadMode))
import Control.Exception (SomeException, try)

-- | Overall task management state
data TaskStat = TaskStat {
  _actives :: [ActiveTask],
  _pool :: [PooledTask],
  _today :: Day,
  _rand :: StdGen
} deriving (Show, Read)


-- | Avtivated tasks
data ActiveTask = ActiveTask {
  _atTask :: Task,
  _atDue :: Day,
  _atFinished :: Maybe Day
 } deriving (Eq, Show, Read)


-- | Tasks which can be potentially activated
data PooledTask = PooledTask {
  _ptTask :: Task,
  _ptDueDays :: Int,
  _ptProp :: Float,
  _ptLastFinished :: Day
 } deriving (Eq, Show, Read)

-- | The core of a task
data Task = Task {
  _tTitle :: String,
  _tDesc :: String,
  _tFactor :: Float
 } deriving (Eq, Show, Read)

makeLenses ''TaskStat
makeLenses ''ActiveTask
makeLenses ''PooledTask
makeLenses ''Task


-- | Adding days using an 'Int'
addDays :: Int -> Day -> Day
addDays i = Time.addDays $ toInteger i

-- | Assuming this is the smallest date used for the tasks
zeroDay :: Day
zeroDay = read "2010-01-01"


-- | 'TaskStat' which contains no or just default values
--
-- The random generator is initialized with seed 0 and will always produce
-- the same values.  Please set to new value before you use it.
emptyTaskStat :: TaskStat
emptyTaskStat = TaskStat [] [] zeroDay (mkStdGen 0)

-- | Add an active task to the state
addActiveTaskM:: (String, String, Float, Int) -> State TaskStat ()
addActiveTaskM (title, desc, factor, dueDays) = do
  t <- use today
  let task = Task title desc factor
      due = addDays dueDays t
      aTask = ActiveTask task due Nothing
  actives %= (\xs -> aTask : xs)

addActiveTask :: (String, String, Float, Int) -> TaskStat -> TaskStat
addActiveTask vals = execState $ addActiveTaskM vals

-- | Adding a pooled task to the state
addPooledTaskM :: (String, String, Float, Int, Float) -> State TaskStat ()
addPooledTaskM (title, desc, factor, dueDay, prop) = do
  let task = Task title desc factor
      pTask = PooledTask task dueDay prop zeroDay
  pool %= (\xs -> pTask : xs)

addPooledTask :: (String, String, Float, Int, Float) -> TaskStat -> TaskStat
addPooledTask vals = execState $ addPooledTaskM vals

-- | Randomly activate pooled tasks
activateM :: State TaskStat ()
activateM = do
  stat <- get
  pTasks <- getPTasksToActivateM
  aTasks <- mapM pTasksToActiveM pTasks
  actives %= (\xs -> aTasks ++ xs)
  return ()

activate :: TaskStat -> TaskStat
activate = execState activateM

-- | Transform a 'PooledTask' to an 'ActiveTask'
pTasksToActiveM :: PooledTask -> State TaskStat ActiveTask
pTasksToActiveM pTask = do
  day <- use today
  let task = view ptTask pTask
      due = addDays (view ptDueDays pTask) day
  return $ ActiveTask task due Nothing


-- | Pick pooled tasks which can be activated
getPTasksToActivateM :: State TaskStat [PooledTask]
getPTasksToActivateM = do
  stat <- get
  let potentialPTasks = toListOf (tasksToActivate stat) stat
  pickPTasksRandomlyM potentialPTasks

-- | Pick tasks from the 'PooledTask' list randomly
pickPTasksRandomlyM :: [PooledTask] -> State TaskStat [PooledTask]
pickPTasksRandomlyM pTasks = filterM shouldTaskBeActivatedM pTasks

-- | Randomly decide if a task should be activated
shouldTaskBeActivatedM :: PooledTask -> State TaskStat Bool
shouldTaskBeActivatedM pTask = do
  f <- randomFloatM
  return $ f < view ptProp pTask

-- | Pick a random value between 0 and 1.
randomFloatM :: State TaskStat Float
randomFloatM = do
  r <- use rand
  let (val, r') = (random r) :: (Float, StdGen)
  rand .= r'
  return val

-- | Traversal to the tasks which can be potentially activated
tasksToActivate :: TaskStat -> Traversal' TaskStat PooledTask
tasksToActivate stat = pool.traverse.(notActivePTasks $ view actives stat)
                             .(notCoolingDown $ view today stat)

-- | Filter only tasks which are not inside the given 'ActiveTask' list
notActivePTasks :: [ActiveTask] -> Traversal' PooledTask PooledTask
notActivePTasks aTasks = filtered $ \pTask ->
  (pTask ^. ptTask . tTitle) `notElem`
        map (\aTask -> aTask ^. atTask . tTitle) aTasks

-- | Filter only tasks which are not cooling down at the moment
notCoolingDown :: Day -> Traversal' PooledTask PooledTask
notCoolingDown day = filtered $ \pTask ->
  let startDay = addDays (pTask ^. ptDueDays) day
   in startDay > pTask ^. ptLastFinished


-- | Get all 'ActiveTask's which are overdue
getOverduesM :: State TaskStat [ActiveTask]
getOverduesM = do
  stat <- get
  day <- use today
  return $ stat ^.. overdues day

getOverdues :: TaskStat -> TaskStat
getOverdues = execState getOverduesM

-- | Traversal to the 'ActiveTask's which are overdue after the given day
overdues :: Day -> Traversal' TaskStat ActiveTask
overdues day = actives.traverse.(filtered $ \aTask ->
  (view atDue aTask) > day)

-- | Remove completed tasks
cleanupM :: State TaskStat ()
cleanupM = do
  stat <- get
  let inactiveTasks = stat ^.. unfinished
  actives .= inactiveTasks

cleanup :: TaskStat -> TaskStat
cleanup = execState cleanupM

-- | Traversal to the unfinished 'ActiveTask's
unfinished :: Traversal' TaskStat ActiveTask
unfinished = actives.traverse.(filtered $ \aTask ->
  (aTask ^. atFinished) /= Nothing)

-- | Mark task with the given title done.
markDoneM :: String -> State TaskStat ()
markDoneM title = do
  day <- use today
  (taskWithTitle title).atFinished .= Just day

markDone :: String -> TaskStat -> TaskStat
markDone title = execState $ markDoneM title

-- | Traversal to task with given title
taskWithTitle :: String -> Traversal' TaskStat ActiveTask
taskWithTitle title = actives.traverse.(filtered $ \aTask ->
  (aTask ^. atTask . tTitle) == title
 )

updateTaskStat :: TaskStat -> IO TaskStat
updateTaskStat ts = do
  stdGen <- newStdGen
  timeNow <- getCurrentTime
  let day = utctDay timeNow
  let ts' = ts & today .~ day
  let ts'' = ts' & rand .~  stdGen
  return ts''

loadTmUnsafe :: String -> IO TaskStat
loadTmUnsafe filename = do
  handle <- openFile filename ReadMode
  !state <- hGetContents handle
  let !tm = (read state) :: TaskStat
  hClose handle
  return tm

loadTm :: String -> IO (Maybe TaskStat)
loadTm filename = do
  eitherTm <- (try $ loadTmUnsafe filename) :: IO (Either SomeException TaskStat)
  case eitherTm of
    Left _ -> return Nothing
    Right tm -> return $ Just tm
