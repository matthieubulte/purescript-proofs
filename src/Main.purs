module Main where

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, logShow)
import Data.Maybe (Maybe)
import Data.Unit (Unit)
import Prelude ((<<<), ($), map)

import Nat(class Nat, S, Z)
import Fin(Fin, intToFin, succ, zero)
import Vec (Vec, cons, empty, get)

--

type N1 = S Z
type N2 = S N1
type N3 = S N2
type N4 = S N3

f1 :: Fin N2
f1 = succ zero

f2 :: Fin N3
f2 = succ f1

f3 :: Fin N4
f3 = succ f2

list :: Vec N3 Int
list = cons 1 $ cons 2 $ cons 3 empty

getFromInt :: forall n. Nat n => Int -> Vec n Int -> Maybe Int
getFromInt ind xs = map (\i -> get i xs) (intToFin ind)

main :: Eff (console :: CONSOLE) Unit
main = logShow $ getFromInt 1 list
