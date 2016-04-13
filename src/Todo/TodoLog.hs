module Todo.TodoLog (
  addActiveTask,
  addPooladTask
                    ) with

import Todo.Todo as Todo
import Todo.Changelog as Log

import qualified Data.Text as T
import Data.Text (Text)

addActiveTask :: (Text, Text, Float, Int) -> 
