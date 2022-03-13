{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Representable where

import Data.Kind (Type)
import Data.Type.Equality

class Representable f where
  type Rep f
  lookup :: Rep f -> f a -> a
  tabulate :: (Rep f -> a) -> f a

data Pair a = Pair a a

instance Representable Pair where
  type Rep Pair = Bool
  lookup False (Pair x _) = x
  lookup True (Pair _ y) = y
  tabulate f = Pair (f False) (f True)

data Nat = Z | S Nat

type One = 'S Z

type Two = 'S ('S Z)

type Three = 'S ('S ('S Z))

data Vector :: Nat -> Type -> Type where
  VNil :: Vector 'Z a
  VCons :: a -> Vector n a -> Vector ('S n) a

deriving instance Show a => Show (Vector n a)

vhead :: Vector ('S n) a -> a
vhead (VCons x xs) = x

vzip :: Vector n a -> Vector n b -> Vector n (a, b)
vzip VNil VNil = VNil
vzip (VCons x xs) (VCons y ys) = VCons (x, y) (vzip xs ys)

type family Plus (m :: Nat) (n :: Nat) where
  Plus Z n = n
  Plus (S m) n = S (Plus m n)

data SNat :: Nat -> Type where
  SZ :: SNat Z
  SS :: SNat n -> SNat (S n)

plusRightId :: SNat n -> Plus n Z :~: n
plusRightId SZ = Refl
plusRightId (SS n) = gcastWith (plusRightId n) Refl

vappend' :: SNat m -> SNat n -> Vector m a -> Vector n a -> Vector (Plus m n) a
vappend' SZ SZ VNil VNil = VNil
vappend' m SZ xs VNil = gcastWith (plusRightId m) xs
vappend' SZ n VNil ys = ys
vappend' m n (VCons x xs) ys = VCons x $ vappend' (spred m) n xs ys

spred :: SNat (S n) -> SNat n
spred (SS n) = n

class IsNat (n :: Nat) where
  nat :: SNat n

instance IsNat Z where
  nat = SZ

instance IsNat n => IsNat (S n) where
  nat = SS nat

vappend ::
  forall n m a.
  (IsNat n, IsNat m) =>
  Vector m a ->
  Vector n a ->
  Vector (Plus m n) a
vappend = vappend' (nat @m) (nat @n)

data Fin :: Nat -> Type where
  FZ :: Fin ('S n)
  FS :: Fin n -> Fin ('S n)
