module Exists where

import Prelude ((<<<))
import Unsafe.Coerce (unsafeCoerce)

import ProofToys (type (~>))

-- This module is the same as https://github.com/purescript/purescript-exists

-- (Exists f) should read as "there is an 'a' such that 'f' holds for 'a'"
-- Exists does not exist at the value level, it is just a type level utility helping
-- us to assert properties about our types.
foreign import data Exists :: (* -> *) -> *

-- The only way to proove that there is an 'a' for which 'f' holds is
-- to actually provide and 'a' and a proof that 'f' holds for 'a'.
mkExists :: forall f a. f a -> Exists f
mkExists = unsafeCoerce

-- When using a proof stating "there is an a such that f holds for a", you don't actually
-- know the value of a, so you must be able to create you next proof r given no more
-- information on a than "f holds for a".
runExists :: forall f r. (forall a. f a -> r) -> Exists f -> r
runExists = unsafeCoerce

mapExists :: forall f g. f ~> g -> Exists f -> Exists g
mapExists f = runExists (mkExists <<< f)
