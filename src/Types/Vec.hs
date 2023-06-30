{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}

module Types.Vec where

import Data.Kind (Type)
import Prelude hiding ((++), map, zip, (!!))
import Data.Functor.Identity (Identity)

data Nat = Zero | Suc Nat
  deriving (Show)

data Vec (n :: Nat) (a :: Type) where
  Nil :: Vec 'Zero a
  Cons :: a -> Vec n a -> Vec ('Suc n) a

deriving instance Show a => Show (Vec n a)

head :: Vec ('Suc n) a -> a
head (Cons x xs) = x

tail :: Vec ('Suc n) a -> Vec n a
tail (Cons x xs) = xs

map :: (a -> b) -> Vec n a -> Vec n b
map _ Nil = Nil
map f (Cons x xs) = Cons (f x) (map f xs)

zip :: Vec n a -> Vec n b -> Vec n (a, b)
zip Nil Nil = Nil
zip (Cons x xs) (Cons y ys) = Cons (x, y) (zip xs ys)

data Index (n :: Nat) where
  IZero :: Index 'Zero
  ISuc :: Index n -> Index ('Suc n)

deriving instance Show (Index x)

(!!) :: Vec ('Suc n) a -> Index n -> a
Cons x _ !! IZero = x
Cons _ xs !! ISuc i = xs !! i

type family AddNat (a :: Nat) (b :: Nat) :: Nat where
  AddNat 'Zero y = y
  AddNat ('Suc x) y = 'Suc (AddNat x y)

(++) :: Vec m a -> Vec n a -> Vec (AddNat m n) a
Nil ++ ys = ys
Cons x xs ++ ys = Cons x (xs ++ ys)

type family Columnar (f :: Type -> Type) (a :: Type) :: Type where
  Columnar Identity x = x
  Columnar f x = f x
