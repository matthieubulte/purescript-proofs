module Nat where

import Prelude ((<<<), ($), (+))
import Data.Either (Either(..))
import Type.Proxy (Proxy(..))

import Leibniz
import ProofToys (type (~>), type (..), Comp(..))

infix 4 type Either as \/

--

-- See the Exists type for information about the background ideas of the Exists type. In this case, the implementation
-- differs a bit because the Nat instance of the subject of the existence proof has to be carried around to be usable
-- at the place where the Existence proof is actually used.
foreign import data ExistsNat :: (* -> *) -> *
foreign import mkExistsNat :: forall f n. Nat n => f n -> ExistsNat f
foreign import runExistsNat :: forall f r. (forall n. Nat n => f n -> r) -> ExistsNat f -> r

mapExistsNat :: forall f g. f ~> g -> ExistsNat f -> ExistsNat g
mapExistsNat f = runExistsNat (mkExistsNat <<< f)

--

type IsSuccOf n = Leibniz n .. S
type HasPred n = ExistsNat (IsSuccOf n)

-- The Nat type class is to be seen as a port of the Nat type to the type level.
--
-- At value level, the usual Nat type only posseses two constructors by being defined as followed:
--
-- data Nat = Z | S Nat
--
-- Being then usable by pattern matching on the constructor as following:
--
-- foo :: Nat -> ...
-- foo n = case n of
--      Z    -> ...
--      S n' -> ...
--
-- To make the Nat type class usable as the usual Nat type, we provide the `split` function, implementing
-- pattern matching on the type level Nat. Pattern matching at the type level, like at the value level,
-- should inform the user about which constructor was used and with which parameters.
--
-- At the type level, we translate the value level pattern matching as followed:
--
-- foo :: forall n. Nat n => ...
-- foo = case split of
--    Left  (p :: n ~ Z)     -> ...
--    Right (p :: HasPred n) -> ...
--
-- Which provides us a proof that n is Z (p :: n ~ Z) or a proof that n was constructed as the successor of
-- another Nat, which is encoded using the HasPred type alias, which represents the proof of
-- ∃n': Nat n' => n ~ S n', which is the same as what we had at type level with the usual Nats.
class Nat n where
  split :: (n ~ Z) \/ (HasPred n)

data Z
instance zeroNat :: Nat Z where
  split = Left refl

data S n
instance succNat :: Nat n => Nat (S n) where
  split = Right (mkExistsNat $ Comp refl)

natToInt :: forall n. Nat n => Proxy n -> Int
natToInt _ = case split of
    Left  (p :: n ~ Z)     -> 0
    Right (p :: HasPred n) -> runExistsNat case_n' p

    where
        case_n' :: forall n'. Nat n' => IsSuccOf n n' -> Int
        case_n' _ = 1 + natToInt (Proxy :: Proxy n')
