module Text.Formatting where

import Data.Show as Data.Show
import Data.Bifunctor (class Bifunctor, lmap, rmap)
import Data.Semigroup ((<>))
import Data.Show (class Show)
import Prelude (class Semigroup, id, ($), (<<<), (>>>))


------------------------------------------------------------
-- Core library.
------------------------------------------------------------

-- | A `String` formatter, like `printf`, but type-safe and composable.
-- |
-- | In general, a function that behaves like `printf "%s: %d"` would
-- | will have the type signature `Format r m (String -> Int -> a)`. In
-- | other words, `r` and `a` are always type variables, and as you build
-- | your formatter the concrete arguments queue up in front of `a`.
-- |
-- | [See the docs for `Format`](#Format) for a technical explanation.

data Format result m f
    = Format ((m -> result) -> f)

compose ::
  forall r s f m.
  (Semigroup m) =>
  Format r m f
  -> Format s m r
  -> Format s m f
compose (Format f) (Format g) =
    Format (\callback -> f $ \fValue -> g $ \gValue -> callback $ fValue <> gValue)

instance formatBifunctor :: Bifunctor (Format r) where
  bimap inputF outputF (Format format) =
    Format (\callback -> outputF (format (inputF >>> callback)))

infix 9 compose as %

identity :: forall m r. Format r m (m -> r)
identity = Format id

apply ::
  forall a r b m.
  Format r m (a -> b)
  -> a
  -> Format r m b
apply (Format format) value =
  Format (\callback -> format callback value)

print :: forall f r. Format r r f -> f
print (Format format) = format id

before ::
  forall r m c b a.
  (a -> b)
  -> Format r m (b -> c)
  -> Format r m (a -> c)
before f (Format format) =
  Format (\callback -> (format callback <<< f))

after ::
  forall r f a b.
  (a -> b)
  -> Format r a f
  -> Format r b f
after = lmap

que :: forall r a b c. (b -> c) -> Format r a b -> Format r a c
que = rmap

toFormatter :: forall r m a. (a -> m) -> Format r m (a -> r)
toFormatter f =
  Format (\callback -> callback <<< f)

------------------------------------------------------------
-- Formatters.
------------------------------------------------------------

show :: forall r a. (Show a) => Format r String (a -> r)
show = Format (\callback value -> callback $ Data.Show.show value)

string :: forall r. Format r String (String -> r)
string = Format (\callback str -> callback str)

int :: forall r. Format r String (Int -> r)
int = show

number :: forall r. Format r String (Number -> r)
number = show

boolean :: forall r. Format r String (Boolean -> r)
boolean = show

s :: forall r. String -> Format r String r
s str = Format (\callback -> callback str )
