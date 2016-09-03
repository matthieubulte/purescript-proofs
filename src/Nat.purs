module Nat where

import Prelude ((<<<), ($), (+))
import Data.Either (Either(..))
import Type.Proxy (Proxy(..))

import Leibniz
import ProofToys (type (~>), type (..), Comp(..))

infix 4 type Either as \/

--

type IsSuccOf n = Leibniz n .. S
type HasPred n = ExistsNat (IsSuccOf n)

class Nat n where
  split :: (n ~ Z) \/ (HasPred n)

foreign import data ExistsNat :: (* -> *) -> *
foreign import mkExistsNat :: forall f n. Nat n => f n -> ExistsNat f
foreign import runExistsNat :: forall f r. (forall n. Nat n => f n -> r) -> ExistsNat f -> r

mapExistsNat :: forall f g. f ~> g -> ExistsNat f -> ExistsNat g
mapExistsNat f = runExistsNat (mkExistsNat <<< f)

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
