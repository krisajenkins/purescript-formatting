module Test.Text.Formatting (tests) where

import Text.Formatting as F
import Data.Array (length)
import Data.Maybe (Maybe(..))
import Data.Show (class Show)
import Data.String (toUpper)
import Prelude (bind, show, (<>), (>>>))
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert (equal)
import Text.Formatting (print, s, string, (%))

tests :: forall e. TestSuite e
tests = do
  suite "Formatting" do
    test "Compose" do
      let helloWorld = s "Hello " % string % s "!"
      equal "Hello Kris!" (print helloWorld "Kris")
    test "Compose - Associativity" do
      let helloWorld = (s "Hello " % string) % s "!"
      equal "Hello Kris!" (print helloWorld "Kris")
    test "Compose - Associativity" do
      let helloWorld = s "Hello " % (string % s "!")
      equal "Hello Kris!" (print helloWorld "Kris")
    test "Show" do
      equal "5 - 10"
        (print (F.show % s " - " % F.show) 5 10)
    test "Int" do
      equal "5"
        (print F.int 5)
    test "Number" do
      equal "123.456"
        (print F.number 123.4560)
    test "Boolean" do
      equal "true"
        (print F.boolean true)
    test "Before" do
      equal "3"
        (print (F.before length F.int) [1, 2, 3])
    test "After" do
      equal "(JUST 5)"
        (print (F.after toUpper F.show) (Just 5))
    test "toFormatter" do
      equal "(JUST 10)"
        (print (F.toFormatter (show >>> toUpper)) (Just 10))
    test "apply" do
      let format = F.int % s " - " % F.boolean
      let partial = F.apply format 3
      equal "3 - false"
        (print partial false)
    test "Real world examples" do
      equal
        "Item (Apples) - £1.89 x 12"
        (print basket (Item "Apples") 1.89 12)
      where
        basket = F.show % s " - £" % F.number % s " x " % F.int

data Item = Item String

instance showItem :: Show Item where
  show (Item name) = "Item (" <> name <> ")"
