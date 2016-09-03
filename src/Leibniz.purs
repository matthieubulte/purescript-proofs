module Leibniz where

import Prelude ((<<<), ($))
import Unsafe.Coerce (unsafeCoerce)

import ProofToys

--

newtype Leibniz a b = Leibniz (forall p. p a -> p b)
infix 4 type Leibniz as ~

coe :: forall f a b. (a ~ b) -> f a -> f b
coe (Leibniz f) = f

coe2 :: forall f a b c. (a ~ b) -> f a c -> f b c
coe2 l = unFlip <<< coe l <<< Flip

coe3 :: forall f a b c d. (a ~ b) -> f a c d -> f b c d
coe3 l = unFlip2 <<< coe l <<< Flip2

cast :: forall a b. (a ~ b) -> a -> b
cast (Leibniz proof) a = unId $ proof (Id a)

refl :: forall a. a ~ a
refl = Leibniz (\x -> x)

trans :: forall a b c. (a ~ b) -> (b ~ c) -> (a ~ c)
trans (Leibniz a_eq_b) (Leibniz b_eq_c) = Leibniz (b_eq_c <<< a_eq_b)

symm :: forall a b. a ~ b -> b ~ a
symm l = unFlip $ coe l (Flip refl)

cong :: forall a b f. a ~ b -> f a ~ f b
cong p = Leibniz (unComp <<< coe p <<< Comp)

-- type constructors in purescript are injective. this is true, as there
-- is no computation done by at the type level when applying a type constructor
-- to parameters (different from haskell's type families), but we can't actually prove it
inj :: forall a b f. f a ~ f b -> a ~ b
inj = unsafeCoerce
