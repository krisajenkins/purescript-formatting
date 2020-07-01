module Test.Main where

import Effect (Effect)
import Prelude
import Test.Text.Formatting as Formatting
import Test.Unit.Main (runTest)

main :: Effect  Unit
main = runTest do
  Formatting.tests
