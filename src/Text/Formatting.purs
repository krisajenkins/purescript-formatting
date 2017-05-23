module Text.Formatting where

import Data.Show as Data.Show
import Control.Semigroupoid (class Semigroupoid)
import Data.Function (id, ($), (<<<))
import Data.Semigroup (class Semigroup, (<>))
import Data.Show (class Show)

------------------------------------------------------------
-- Core library.
------------------------------------------------------------

-- | A `String` formatter, like `printf`, but type-safe and composable.
-- |
-- | As an example, a function that behaves like `printf "%s: %d"` would
-- | will have the type signature `Format String r (String -> Int -> r)`. In
-- | you build up a function that eventually yields a `r`. That
-- | remains a variable, because you might want to to append other
-- | arguments, but the first part - `Format String` tells you that
-- | when this formatter finally gets used, it must eventually yield a
-- | string.
-- |
-- | ## Examples:
-- |
-- | ``` purescript
-- | import Text.Formatting (print, s, string)
-- | ```
-- |
-- | Build up a `Format`, composing with `<<<`.
-- | ``` purescript
-- | greeting :: Format String (String -> String)
-- | greeting = s "Hello " <<< string <<< s "!"
-- | ```
-- |
-- | Convert it to a function with `print`:
-- | ``` purescript
-- | greet :: String -> String
-- | greet = print greeting
-- | ```
-- |
-- | Then use it:
-- | ``` purescript
-- | message1 :: String
-- | message1 = greet "Kris"
-- | --> message1 == "Hello Kris!"
-- | ```
-- |
-- | Or more often, use it directly:
-- | ``` purescript
-- | message2 :: String
-- | message2 = print greeting "Kris"
-- | --> message2 == "Hello Kris!"
-- | ```
-- |
-- | Extend it just by composing more onto it:
-- | ``` purescript
-- | inbox :: forall r. Format String r (String -> Int -> r)
-- | inbox = greeting <<< s " You have " <<< F.int <<< s " new messages."
-- | ```
-- |
-- | `print` still makes it into function:
-- | ``` purescript
-- | welcome :: String -> Int -> String
-- | welcome = print inbox
-- | ```
-- |
-- | Or again, call it in one go:
-- | ``` purescript
-- | message3 :: String
-- | message3 = print inbox "Kris" 3
-- | --> message3 == "Hello Kris! You have 3 new messages."
-- | ```

data Format monoid result f
    = Format ((monoid -> result) -> f)

composeFormat ::
  forall r s m f.
  Semigroup m
  => Format m r f
  -> Format m s r
  -> Format m s f
composeFormat (Format f) (Format g) =
  Format (\callback -> f $ \fValue -> g $ \gValue -> callback $ fValue <> gValue)

-- | Note to interested readers: `Format` should be a `Semigroupoid` -
-- | and hence composable with `<<<` for any format of type `forall m r
-- | f. Semigroupoid m => Format m r f`
-- |
-- | However, I don't know how to persuade PureScript of that. Or even
-- | if it's valid to say, "This is a member of that category,
-- | provided you meet my extra constraints..."
-- |
-- | Nevertheless, for the most common format - the one that yields
-- | `String`s, it's composable. And that probably all most people
-- | will care about.
instance formatSemigroupoid :: Semigroupoid (Format String) where
  compose = composeFormat

-- | Turns a `Format` into the underlying function it has built up.
-- | Call this when you're ready to apply all the arguments and
-- | generate an `r` (usually a `String`).
print :: forall f r. Format r r f -> f
print (Format format) = format id

-- | Apply the first argument of the formatter, without unwrapping it
-- | to a plain ol' function.
apply ::
  forall r m a b.
  Format m r (a -> b)
  -> a
  -> Format m r b
apply (Format format) value =
  Format (\callback -> format callback value)

-- | Turn a function into a `Format`.
toFormatter :: forall r m a. (a -> m) -> Format m r (a -> r)
toFormatter f =
  Format (\callback -> callback <<< f)

-- | Modify a `Format` so that this (contravariant) function is called
-- | on its first argument.
-- |
-- | ## Example:
-- | ``` purescript
-- | import Text.Formatting as F
-- | print (F.before length F.int) [1, 2, 3]
-- | --> "3"
-- | ```
before ::
  forall r m a b c.
  (b -> a)
  -> Format m r (a -> c)
  -> Format m r (b -> c)
before f (Format format) =
  Format (\callback -> format callback <<< f)

-- | Modify a `Format` so that this function is called on its final result.
-- | ## Example:
-- | ``` purescript
-- | import Text.Formatting as F
-- | print (F.after toUpper show) (Just 3)
-- | --> "(JUST 3)"
-- | ```
after :: forall r m n f. (m -> n) -> Format m r f -> Format n r f
after f (Format format) =
  Format (\callback -> format (callback <<< f))

------------------------------------------------------------
-- Formatters.
------------------------------------------------------------

-- | Accept any `Show`able argument.
show :: forall r a. Show a => Format String r (a -> r)
show = Format (\callback value -> callback $ Data.Show.show value)

-- | Accept a `String`.
string :: forall r. Format String r (String -> r)
string = Format (\callback str -> callback str)

-- | Accept an `Int`.
int :: forall r. Format String r (Int -> r)
int = show

-- | Accept a `Number`.
number :: forall r. Format String r (Number -> r)
number = show

-- | Accept a `Boolean`.
boolean :: forall r. Format String r (Boolean -> r)
boolean = show

-- | Insert a fixed string.
s :: forall r. String -> Format String r r
s str = Format (\callback -> callback str)
