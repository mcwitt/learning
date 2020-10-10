{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}

-- | Exercises from Andres Löh's intro to datatype-generic
-- programming, presented at ZuriHac 2020
-- https://github.com/well-typed/gp-zurihac-2020/blob/master/GP.pdf
module Generics where

import Data.Monoid

data Tree a = Leaf a | Node (Tree a) (Tree a)

class Generic a where
  type Rep a
  from :: a -> Rep a
  to :: Rep a -> a

newtype Wrap a = Wrap a

instance Generic (Tree a) where
  type Rep (Tree a) = Either (Wrap a) (Wrap (Tree a), Wrap (Tree a))

  from :: Tree a -> Rep (Tree a)
  from (Leaf x) = Left (Wrap x)
  from (Node x y) = Right (Wrap x, Wrap y)

  to :: Rep (Tree a) -> Tree a
  to (Left (Wrap x)) = Leaf x
  to (Right (Wrap x, Wrap y)) = Node x y

eq :: (Generic a, GEq (Rep a)) => a -> a -> Bool
eq x y = geq (from x) (from y)

class GEq a where
  geq :: a -> a -> Bool

instance Eq a => GEq (Wrap a) where
  geq (Wrap x) (Wrap y) = x == y

instance (GEq a, GEq b) => GEq (Either a b) where
  geq (Left x) (Left y) = geq x y
  geq (Right x) (Right y) = geq x y
  geq _ _ = False

instance (GEq a, GEq b) => GEq (a, b) where
  geq (x1, y1) (x2, y2) = geq x1 x2 && geq y1 y2

instance Eq a => Eq (Tree a) where
  (==) = eq

t1, t2, t3 :: Tree Int
t1 = Node (Leaf 42) (Leaf 43) -- (Leaf 42, Leaf 43)
t2 = Node (Leaf 42) (Leaf 43)
t3 = Node (Leaf 42) (Node (Leaf 1) (Leaf 2))

-------------------------------------------------------------------------------
-- Exercise 1

data Term
  = App Term Term
  | Abs String Term
  | Var String

instance Generic Term where
  type
    Rep Term =
      Either
        (Wrap Term, Wrap Term)
        (Either (Wrap String, Wrap Term) (Wrap String))

  from :: Term -> Rep Term
  from (App t t') = Left (Wrap t, Wrap t')
  from (Abs s t) = Right (Left (Wrap s, Wrap t))
  from (Var s) = Right (Right (Wrap s))

  to :: Rep Term -> Term
  to (Left (Wrap t, Wrap t')) = App t t'
  to (Right (Left (Wrap s, Wrap t))) = Abs s t
  to (Right (Right (Wrap s))) = Var s

instance Eq Term where
  (==) = eq

term1, term2, term3 :: Term
term1 = App (Abs "foo" (Var "bar")) (Var "baz")
term2 = App (Abs "foo" (Var "bar")) (Var "quux")
term3 = Var "bum"

-------------------------------------------------------------------------------
-- Exercise 2

cmp :: (Generic a, GCmp (Rep a)) => a -> a -> Ordering
cmp x y = gcmp (from x) (from y)

class GCmp a where
  gcmp :: a -> a -> Ordering

instance Ord a => GCmp (Wrap a) where
  gcmp (Wrap x) (Wrap y) = compare x y

instance (GCmp a, GCmp b) => GCmp (Either a b) where
  gcmp (Left x) (Left y) = gcmp x y
  gcmp (Right x) (Right y) = gcmp x y
  gcmp (Left x) (Right y) = LT
  gcmp (Right x) (Left y) = GT

instance (GCmp a, GCmp b) => GCmp (a, b) where
  gcmp (x1, y1) (x2, y2) = case gcmp x1 x2 of
    EQ -> gcmp y1 y2
    c -> c

instance Ord Term where
  compare = cmp

-------------------------------------------------------------------------------
-- Exercise 3

-- data Color = Red | Green | Blue  -- from GEnum example
data Color
  = Red
  | Green
  | Blue
  | Gray Int
  deriving (Show)

instance Generic Color where
  type Rep Color = Either () (Either () (Either () (Wrap Int)))

  from Red = Left ()
  from Green = Right (Left ())
  from Blue = Right (Right (Left ()))
  from (Gray s) = Right (Right (Right (Wrap s)))

  to (Left ()) = Red
  to (Right (Left ())) = Green
  to (Right (Right (Left ()))) = Blue
  to (Right (Right (Right (Wrap s)))) = Gray s

data Color1
  = Red1
  | Green1
  | Blue1
  | RGB1 Int Int Int
  deriving (Show)

instance Generic Color1 where
  type Rep Color1 = Either () (Either () (Either () (Wrap Int, (Wrap Int, Wrap Int))))

  from Red1 = Left ()
  from Green1 = Right (Left ())
  from Blue1 = Right (Right (Left ()))
  from (RGB1 r g b) = Right (Right (Right (Wrap r, (Wrap b, Wrap g))))

  to (Left ()) = Red1
  to (Right (Left ())) = Green1
  to (Right (Right (Left ()))) = Blue1
  to (Right (Right (Right (Wrap r, (Wrap b, Wrap g))))) = RGB1 r g b

instance GEq () where
  geq () () = True

instance Eq Color where
  (==) = eq

baseCases :: (Generic a, GBaseCases (Rep a)) => [a]
baseCases = to <$> gBaseCases

class GBaseCases a where
  gBaseCases :: [a]

instance GBaseCases () where
  gBaseCases = [()]

instance GBaseCases (Wrap a) where
  gBaseCases = []

instance GBaseCases (a, b) where
  gBaseCases = []

instance (GBaseCases a, GBaseCases b) => GBaseCases (Either a b) where
  gBaseCases = (Left <$> gBaseCases) ++ (Right <$> gBaseCases)

-------------------------------------------------------------------------------
-- Exercise 4

memp :: (Generic a, GMemp (Rep a)) => a
memp = to gmemp

class GMemp a where
  gmemp :: a

instance Monoid a => GMemp (Wrap a) where
  gmemp = Wrap mempty

instance (GMemp a, GMemp b) => GMemp (a, b) where
  gmemp = (gmemp, gmemp)

data Vec3 = V3 (Sum Int) (Sum Int) (Sum Int) deriving (Show)

instance Generic Vec3 where
  type Rep Vec3 = (Wrap (Sum Int), (Wrap (Sum Int), Wrap (Sum Int)))
  from (V3 x y z) = (Wrap x, (Wrap y, Wrap z))
  to (Wrap x, (Wrap y, Wrap z)) = V3 x y z

-------------------------------------------------------------------------------
-- Exercise 5

total :: (Generic a, GTotal (Rep a)) => a -> Int
total = gtotal . from

class GTotal a where
  gtotal :: a -> Int

instance GTotal (Wrap Int) where
  gtotal (Wrap n) = n

instance (GTotal a, GTotal b) => GTotal (a, b) where
  gtotal (a, b) = gtotal a + gtotal b

instance (GTotal a, GTotal b) => GTotal (Either a b) where
  gtotal (Left a) = gtotal a
  gtotal (Right b) = gtotal b

data Outcome = Success {numProcessed :: Int} | Failure {numSuccess :: Int, numFailure :: Int}

instance Generic Outcome where
  type Rep Outcome = Either (Wrap Int) (Wrap Int, Wrap Int)

  from (Success n) = Left (Wrap n)
  from (Failure ns nf) = Right (Wrap ns, Wrap nf)

  to (Left (Wrap n)) = Success n
  to (Right (Wrap ns, Wrap nf)) = Failure ns nf
