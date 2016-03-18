{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE NoMonomorphismRestriction #-}

import Test.Hspec
import Todo.Todo
import Control.Lens
import Data.Time
import System.Random
import Control.Monad.State

tstM :: State TaskStat ()
tstM = do
  actives .= []
  pool .= []
  today .= read "2010-01-01"
  rand .= mkStdGen 0

tst = execState tstM emptyTaskStat

main :: IO ()
main = hspec $ do
  describe "The task handling" $ do
    it "should be able to add an active task" $ do
      let tst' = execState (addActiveTask ("title", "desc", 1.2, 3)) tst
      length (tst' ^. actives) `shouldBe` 1
      let aTask : _ = tst' ^. actives
      (aTask ^. atTask . tTitle) `shouldBe` "title"
      (aTask ^. atTask . tDesc) `shouldBe` "desc"
      (aTask ^. atTask . tFactor) `shouldBe` 1.2
      (aTask ^. atDue) `shouldBe` (read "2010-01-04")
