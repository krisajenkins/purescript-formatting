module Test.Main where

import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Prelude
import Test.Text.Formatting as Formatting
import Test.Unit.Console (TESTOUTPUT)
import Test.Unit.Main (runTest)

main :: forall eff.
  Eff ( console :: CONSOLE , testOutput :: TESTOUTPUT , avar :: AVAR | eff ) Unit
main = runTest do
  Formatting.tests
