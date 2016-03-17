{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}

module Todo.Todo (
  -- * Types
  TaskStat,
  ActiveTask,
  PooledTask,
  Task,

  -- * TaskStat State monad functions
  emptyTaskStat

) where

import System.Random (StdGen, random, mkStdGen)
import Data.Time (Day)
import qualified Data.Time as Time
import Control.Lens
import Control.Monad.State.Lazy

data TaskStat = TaskStat {
  _actives :: [ActiveTask],
  _pool :: [PooledTask],
  _today :: Day,
  _rand :: StdGen
}


data ActiveTask = ActiveTask {
  _atTask :: Task,
  _atDue :: Day,
  _atFinished :: Maybe Day
}

data PooledTask = PooledTask {
  _ptTask :: Task,
  _ptDueDays :: Int,
  _ptProp :: Float,
  _ptLastFinished :: Day
}

data Task = Task {
  _tTitle :: String,
  _tDesc :: String,
  _tFactor :: Float
}

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


emptyTaskStat = TaskStat [] [] zeroDay (mkStdGen 0)

-- | Add an active task to the state
addActiveTask :: (String, String, Float, Int) -> State TaskStat ()
addActiveTask (title, desc, factor, dueDays) = do
  t <- use today
  let task = Task title desc factor
      due = addDays dueDays t
      aTask = ActiveTask task due Nothing
  actives %= (\xs -> aTask : xs)

-- | Adding a pooled task to the state
addPooledTask :: (String, String, Float, Int, Float) -> State TaskStat ()
addPooledTask (title, desc, factor, dueDay, prop) = do
  let task = Task title desc factor
      pTask = PooledTask task dueDay prop zeroDay
  pool %= (\xs -> pTask : xs)

-- | Randomly activate pooled tasks
activate :: State TaskStat ()
activate = do
  stat <- get
  pTasks <- getPTasksToActivate
  aTasks <- mapM pTasksToActive pTasks
  actives %= (\xs -> aTasks ++ xs)
  return ()

-- | Transform a 'PooledTask' to an 'ActiveTask'
pTasksToActive :: PooledTask -> State TaskStat ActiveTask
pTasksToActive pTask = do
  day <- use today
  let task = view ptTask pTask
      due = addDays (view ptDueDays pTask) day
  return $ ActiveTask task due Nothing

-- | Pick pooled tasks which can be activated
getPTasksToActivate :: State TaskStat [PooledTask]
getPTasksToActivate = do
  stat <- get
  let potentialPTasks = toListOf (tasksToActivate stat) stat
  pickPTasksRandomly potentialPTasks

-- | Pick tasks from the 'PooledTask' list randomly
pickPTasksRandomly :: [PooledTask] -> State TaskStat [PooledTask]
pickPTasksRandomly pTasks = filterM shouldTaskBeActivated pTasks

-- | Randomly decide if a task should be activated
shouldTaskBeActivated :: PooledTask -> State TaskStat Bool
shouldTaskBeActivated pTask = do
  f <- randomFloat
  return $ f < view ptProp pTask

-- | Pick a random value between 0 and 1.
randomFloat :: State TaskStat Float
randomFloat = do
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
  (pTask ^. ptTask . tTitle) `elem`
        map (\aTask -> aTask ^. atTask . tTitle) aTasks

-- | Filter only tasks which are not cooling down at the moment
notCoolingDown :: Day -> Traversal' PooledTask PooledTask
notCoolingDown day = filtered $ \pTask ->
  let startDay = addDays (pTask ^. ptDueDays) day
   in startDay > pTask ^. ptLastFinished


-- | Get all 'ActiveTask's which are overdue
getOverdues :: State TaskStat [ActiveTask]
getOverdues = do
  stat <- get
  day <- use today
  return $ stat ^.. overdues day

-- | Traversal to the 'ActiveTask's which are overdue if today is the given day
overdues :: Day -> Traversal' TaskStat ActiveTask
overdues day = actives.traverse.(filtered $ \aTask ->
  (view atDue aTask) > day)

-- | Remove completed tasks
cleanup :: State TaskStat ()
cleanup = do
  stat <- get
  let inactiveTasks = stat ^.. unfinished
  actives .= inactiveTasks

-- | Traversal to the unfinished 'ActiveTask's
unfinished :: Traversal' TaskStat ActiveTask
unfinished = actives.traverse.(filtered $ \aTask ->
  (aTask ^. atFinished) /= Nothing)

-- | Mark task with the given title done.
markDone :: String -> State TaskStat ()
markDone title = do
  day <- use today
  (taskWithTitle title).atFinished .= Just day

-- | Traversal to task with given title
taskWithTitle :: String -> Traversal' TaskStat ActiveTask
taskWithTitle title = actives.traverse.(filtered $ \aTask ->
  (aTask ^. atTask . tTitle) == title
 )
