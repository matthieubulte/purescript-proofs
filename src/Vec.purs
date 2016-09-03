module Vec where

import Unsafe.Coerce (unsafeCoerce)
import Prelude ((<<<), ($))

import Leibniz(type (~), symm, trans, inj, coe3, refl)
import Exists(Exists, runExists, mkExists, mapExists)
import Nat(S, Z, runExistsNat)
import Fin(Fin, FinS(..))

--

data VecS n a n' = Vempty             (n ~ Z)
                 | Vcons a (Vec n' a) (n ~ S n')

type Vec n a = Exists (VecS n a)

empty :: forall a. Vec Z a
empty = mkExists (Vempty refl)

cons :: forall n a. a -> Vec n a -> Vec (S n) a
cons x xs = mkExists $ Vcons x xs refl

castLength :: forall n n' a. (n ~ n') -> Vec n a -> Vec n' a
castLength proof = mapExists (coe3 proof)

vhead :: forall n a. Vec (S n) a -> a
vhead = runExists __case
  where
    __case :: forall n'. VecS (S n) a n' -> a
    __case (Vcons x _ _) = x
    __case _  = unsafeCoerce "impossible"

vtail :: forall n a. Vec (S n) a -> Vec n a
vtail = runExists __case
  where
    __case :: forall n'. VecS (S n) a n' -> Vec n a
    __case (Vcons _ xs sn_is_sn') = castLength (symm $ inj sn_is_sn') xs
    __case _ = unsafeCoerce "impossible"

--

get :: forall a n. Fin n -> Vec n a -> a
get i xs = runExistsNat __case_i i
  where
    __case_i :: forall n'. FinS n n' -> a
    __case_i (Fz _) = runExists __case_xs xs
      where
        __case_xs :: forall n''. VecS n a n'' -> a
        __case_xs (Vempty _) = unsafeCoerce "impossible"
        __case_xs (Vcons x _ _) = x

    __case_i (Fs i' n_s_n') = runExists __case_xs xs
      where
        __case_xs :: forall n''. VecS n a n'' -> a
        __case_xs (Vempty _) = unsafeCoerce "impossible"
        __case_xs (Vcons _ xs' n_s_n'') = get i' (castLength n''_is_n' xs')
          where
            n''_is_n' :: (n'' ~ n')
            n''_is_n' = symm <<< inj $ trans (symm n_s_n') n_s_n''
