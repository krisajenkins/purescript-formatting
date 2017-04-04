module Text.Formatting where

import Data.Semigroup (class Semigroup)
import Data.Show (class Show)
import Data.Show as Data.Show
import Prelude (id, ($), (<<<), (<>), (>>>))

------------------------------------------------------------
-- Core library.
------------------------------------------------------------

data FormatT m r a
    = FormatT ((m -> r) -> a)

type Format = FormatT String

-- TODO Is this a semigroupoid, or is it merely semigroupoid-oid?
--compose :: forall a b c. Format b a -> Format c b -> Format c a
compose ::
  forall t33 t36 t38 t40.
  (Semigroup t40) =>
  FormatT t40 t33 t38
           -> FormatT t40 t36 t33
           -> FormatT t40 t36 t38
compose (FormatT f) (FormatT g) =
    FormatT (\callback -> f $ \strF -> g $ \strG -> callback $ strF <> strG)

infix 9 compose as %

apply ::
  forall a r b m.
  FormatT m r (a -> b)
  -> a
  -> FormatT m r b
apply (FormatT format) value =
    FormatT (\callback -> format callback value)

print :: forall r a. FormatT r r a -> a
print (FormatT format) = format id

-- TODO I have a suspicion that there's some known category out there
-- that `before` (and possibly `after`) belong to. `before` smells a bit
-- like a contravariant functor... 🤔
before ::
  forall m r a b c.
  (a -> b)
  -> FormatT m r (b -> c)
  -> FormatT m r (a -> c)
before f (FormatT format) =
    FormatT (\callback -> f >>> format callback)

after ::
  forall r a n m.
  (m -> n)
  -> FormatT m r a
  -> FormatT n r a
after f (FormatT format) =
    FormatT (\callback -> format $ callback <<< f)

toFormatter ::
  forall r m a.
  (a -> m)
  -> FormatT m r (a -> r)
toFormatter f =
    FormatT (\callback -> callback <<< f)

------------------------------------------------------------
-- Formatters.
------------------------------------------------------------

show :: forall r a. Show a => Format r (a -> r)
show = FormatT (\callback -> callback <<< Data.Show.show)

identity :: forall m r. FormatT m r (m -> r)
identity = FormatT id

string :: forall r. Format r (String -> r)
string = identity

int :: forall r. Format r (Int -> r)
int = show

number :: forall r. Format r (Number -> r)
number = show

boolean :: forall r. Format r (Boolean -> r)
boolean = show

s :: forall r m. m -> FormatT m r r
s str = FormatT (\callback -> callback str)
