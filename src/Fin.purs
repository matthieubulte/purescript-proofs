module Fin where

import Prelude ((<<<), ($), (<$>), (<), (-), otherwise)
import Data.Maybe (Maybe(..))
import Data.Either (Either(..))

import ProofToys (unComp)
import Leibniz(type (~), refl, coe2, symm)
import Nat(S, Z, class Nat, split, ExistsNat, mkExistsNat, runExistsNat, mapExistsNat, IsSuccOf, HasPred)

--

data FinS n n' = Fz          (n ~ S n')
               | Fs (Fin n') (n ~ S n')

type Fin n = ExistsNat (FinS n)

zero :: forall n. Nat n => Fin (S n)
zero = mkExistsNat (Fz refl)

succ :: forall n. Nat n => Fin n -> Fin (S n)
succ n = mkExistsNat (Fs n refl)

intToFin :: forall n. Nat n => Int -> Maybe (Fin n)
intToFin x | x < 0     = Nothing
           | otherwise = case split of
                Left  (p :: n ~ Z)     -> Nothing
                Right (p :: HasPred n) -> runExistsNat (case_n' x) p

          where
            case_n' :: forall n'. Nat n' => Int -> (IsSuccOf n) n' -> Maybe (Fin n)
            case_n' 0 p = Just $ cast_sn' p zero
            case_n' y p = cast_sn' p <$> succ <$> intToFin (y - 1)

            cast_sn' :: forall m m'. (IsSuccOf m) m' -> Fin (S m') -> Fin m
            cast_sn' p = mapExistsNat (coe2 $ symm (unComp p))
