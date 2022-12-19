{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Functor.Barbie where

--------------------------------------------------------------------------------

import Data.Kind
import Data.Data
import GHC.TypeLits
import GHC.Generics
import Data.Coerce

--------------------------------------------------------------------------------

data family Param (n :: Nat) (a :: k) :: k

type family Indexed (t :: k) (i :: Nat) :: k where
  Indexed (t a) i = Indexed t (i + 1) (Param i a)
  Indexed t _     = t

type family FilterIndex (n :: Nat) (t :: k) :: k where
  FilterIndex n (t (Param n a)) = FilterIndex n t (Param n a)
  FilterIndex n (t (Param _ a)) = FilterIndex n t a
  FilterIndex _ t = t

newtype Rec (p :: Type) a x = Rec { unRec :: K1 R a x }

type family Zip (f :: Type -> Type) (g :: Type -> Type) :: Type -> Type where
  Zip (M1 mt m s) (M1 mt m t)
    = M1 mt m (Zip s t)
  Zip (l :+: r) (l' :+: r')
    = Zip l l' :+: Zip r r'
  Zip (l :*: r) (l' :*: r')
    = Zip l l' :*: Zip r r'
  Zip (Rec0 f) (Rec0 a)
    = Rec f a
  Zip U1 U1
    = U1
  Zip V1 V1
    = V1

class
  ( Coercible (Rep a) (RepN a)
  , Generic a
  ) => GenericN (a :: Type) where
  type family RepN (a :: Type) :: Type -> Type
  type instance RepN a = Zip (Rep (Indexed a 0)) (Rep a)
  toN :: RepN a x -> a
  fromN :: a -> RepN a x

instance
  ( Coercible (Rep a) (RepN a)
  , Generic a
  ) => GenericN a where
  toN :: forall x. RepN a x -> a
  toN   = coerce (to :: Rep a x -> a)
  {-# INLINE toN #-}

  fromN :: forall x. a -> RepN a x
  fromN = coerce (from :: a -> Rep a x)
  {-# INLINE fromN #-}

class
  ( Coercible (Rep a) (RepP n a)
  , Generic a
  ) => GenericP (n :: Nat) (a :: Type) where
  type family RepP n a :: Type -> Type
  type instance RepP n a = Zip (Rep (FilterIndex n (Indexed a 0))) (Rep a)
  toP :: Proxy n -> RepP n a x -> a
  fromP :: Proxy n -> a -> RepP n a x

instance
  ( Coercible (Rep a) (RepP n a)
  , Generic a
  ) => GenericP (n :: Nat) (a :: Type) where
  toP :: forall x . Proxy n -> RepP n a x -> a
  toP _ = coerce (to :: Rep a x -> a)
  {-# INLINE toP #-}

  fromP :: forall x . Proxy n -> a -> RepP n a x
  fromP _ = coerce (from :: a -> Rep a x)
  {-# INLINE fromP #-}

--------------------------------------------------------------------------------

class FunctorB (b :: (k -> Type) -> Type) where
  bmap :: (forall a . f a -> g a) -> b f -> b g
  default bmap
    :: forall f g
    .  CanDeriveFunctorB b f g
    => (forall a . f a -> g a) -> b f -> b g
  bmap = gbmapDefault

type CanDeriveFunctorB b f g
  = ( GenericP 0 (b f)
    , GenericP 0 (b g)
    , GFunctorB 0 f g (RepP 0 (b f)) (RepP 0 (b g))
    )

-- | Default implementation of 'bmap' based on 'Generic'.
gbmapDefault
  :: CanDeriveFunctorB b f g
  => (forall a . f a -> g a) -> b f -> b g
gbmapDefault f
  = toP (Proxy @0) . gbmap (Proxy @0) f . fromP (Proxy @0)
{-# INLINE gbmapDefault #-}

--------------------------------------------------------------------------------

class GFunctorB (n :: Nat) f g repbf repbg where
  gbmap :: Proxy n -> (forall a. f a -> g a) -> repbf x -> repbg x

instance (GFunctorB n f g bf bg) => GFunctorB n f g (M1 i c bf) (M1 i c bg) where
  gbmap pn h = M1 . gbmap pn h . unM1
  {-# INLINE gbmap #-}

instance GFunctorB n f g V1 V1 where
  gbmap _ _ _ = undefined

instance GFunctorB n f g U1 U1 where
  gbmap _ _ = id
  {-# INLINE gbmap #-}

instance
  ( GFunctorB n f g l l' , GFunctorB n f g r r') => GFunctorB n f g (l :*: r) (l' :*: r') where
  gbmap pn h (l :*: r) = gbmap pn h l :*: gbmap pn h r
  {-# INLINE gbmap #-}

instance
  ( GFunctorB n f g l l'
  , GFunctorB n f g r r'
  ) => GFunctorB n f g (l :+: r) (l' :+: r')
  where
  gbmap pn h = \case
    L1 l -> L1 (gbmap pn h l)
    R1 r -> R1 (gbmap pn h r)
  {-# INLINE gbmap #-}

instance GFunctorB n f g (Rec (Param n f a') (f a)) (Rec (Param n g a') (g a)) where
  gbmap _ h (Rec (K1 fa)) = Rec (K1 (h fa))
  {-# INLINE gbmap #-}

instance (Functor h) => GFunctorB n f g (Rec (h (Param n f a')) (h (f a))) (Rec (h (Param n g a')) (h (g a))) where
  gbmap _ h (Rec (K1 hfa)) = Rec (K1 (h <$> hfa))
  {-# INLINE gbmap #-}

instance GFunctorB n f g (Rec x x) (Rec x x) where
  gbmap _ _ = id
  {-# INLINE gbmap #-}

--------------------------------------------------------------------------------

newtype IdB f = IdB { value :: f Bool }
  deriving (Generic)

deriving instance FunctorB IdB
