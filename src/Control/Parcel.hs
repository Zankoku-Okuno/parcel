{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Control.Parcel
  ( Parcel(..)
  ) where

import Control.Monad.Cont (Cont,cont,runCont)
import Control.Monad.Reader (Reader,MonadReader(reader),runReader)
import Data.Functor.Compose (Compose(..))
import Data.Functor.Const (Const(..))
import Data.Functor.Contravariant (Op(..),Predicate(..),Comparison(..),Equivalence(..))
import Data.Functor.Identity (Identity(..))
import Data.Monoid (All(..),Any(..))
import Data.Monoid (Alt(..),Ap(..))
import Data.Monoid (Endo(..))
import Data.Monoid (First(..),Last(..))
import Data.Monoid (Sum(..),Product(..))
import Data.Ord (Down(..))

import qualified Data.Functor.Product as Functor
import qualified Data.Functor.Sum as Functor


-- |When you don't want to deal with unpacking the newtypes that can get
--  introduced when controlling the behavoir through typeclasses.
--
-- Law 1: 'pkg . unpkg === id'
-- Law 2: 'pkg' and 'unpkg' run in constant time.
--
-- Example:
-- 'foldMap Sum [1, 2, 3] ==> Sum 6'
-- '(unpkg . foldMap Sum) [1, 2, 3] ==> 6'
class Parcel a b where
  -- | Must run in O(1), but preferably O(0)
  pkg :: b -> a
  -- | Must run in O(1), but preferably O(0)
  unpkg :: a -> b

-- NOTE: I've not included '(Coercible a b) => Parcel a b', since that seems like a very different use-case

instance Parcel (Identity a) a where
  pkg = Identity
  unpkg = runIdentity

instance Parcel (Const a b) a where
  pkg = Const
  unpkg = getConst

instance Parcel (Compose f g a) (f (g a)) where
  pkg = Compose
  unpkg = getCompose

instance Parcel (Functor.Product f g a) (f a, g a) where
  pkg = uncurry Functor.Pair
  unpkg (Functor.Pair x y) = (x, y)

instance Parcel (Functor.Sum f g a) (Either (f a) (g a)) where
  pkg (Left x) = Functor.InL x
  pkg (Right y) = Functor.InR y
  unpkg (Functor.InL x) = Left x
  unpkg (Functor.InR x) = Right x

instance Parcel (Endo a) (a -> a) where
  pkg = Endo
  unpkg = appEndo

instance Parcel (Op a b) (b -> a) where
  pkg = Op
  unpkg = getOp

instance Parcel (Predicate a) (a -> Bool) where
  pkg = Predicate
  unpkg = getPredicate

instance Parcel (Comparison a) (a -> a -> Ordering) where
  pkg = Comparison
  unpkg = getComparison

instance Parcel (Equivalence a) (a -> a -> Bool) where
  pkg = Equivalence
  unpkg = getEquivalence

instance Parcel All Bool where
  pkg = All
  unpkg = getAll

instance Parcel Any Bool where
  pkg = Any
  unpkg = getAny

instance Parcel (Sum a) a where
  pkg = Sum
  unpkg = getSum

instance Parcel (First a) (Maybe a) where
  pkg = First
  unpkg = getFirst

instance Parcel (Last a) (Maybe a) where
  pkg = Last
  unpkg = getLast

instance Parcel (Product a) a where
  pkg = Product
  unpkg = getProduct

instance Parcel (Alt f a) (f a) where
  pkg = Alt
  unpkg = getAlt

instance Parcel (Ap f a) (f a) where
  pkg = Ap
  unpkg = getAp

instance Parcel (Down a) a where
  pkg = Down
  unpkg = getDown

instance Parcel (Reader r a) (r -> a) where
  pkg = reader
  unpkg = runReader

instance Parcel (Cont r a) ((a -> r) -> r) where
  pkg = cont
  unpkg = runCont
