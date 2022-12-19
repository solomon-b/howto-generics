module Data.Functor where

--------------------------------------------------------------------------------

import GHC.Generics

--------------------------------------------------------------------------------

class GFunctor s t a b where
  gfmap :: (a -> b) -> s x -> t x

instance GFunctor s t a b => GFunctor (M1 k m s) (M1 k m t) a b where
  gfmap ::
    GFunctor s t a b =>
    (a -> b) ->
    M1 k m s x ->
    M1 k m t x
  gfmap f = M1 . gfmap f . unM1

instance
  ( GFunctor l l' a b,
    GFunctor r r' a b
  ) =>
  GFunctor (l :+: r) (l' :+: r') a b
  where
  gfmap ::
    (GFunctor l l' a b, GFunctor r r' a b) =>
    (a -> b) ->
    (:+:) l r x ->
    (:+:) l' r' x
  gfmap f (L1 l) = L1 (gfmap f l)
  gfmap f (R1 r) = R1 (gfmap f r)

instance
  ( GFunctor l l' a b,
    GFunctor r r' a b
  ) =>
  GFunctor (l :*: r) (l' :*: r') a b
  where
  gfmap ::
    (GFunctor l l' a b, GFunctor r r' a b) =>
    (a -> b) ->
    (:*:) l r x ->
    (:*:) l' r' x
  gfmap f (l :*: r) = gfmap f l :*: gfmap f r

instance GFunctor U1 U1 a b where
  gfmap :: (a -> b) -> U1 x -> U1 x
  gfmap _ = id

instance {-# INCOHERENT #-} GFunctor (Rec0 a) (Rec0 b) a b where
  gfmap :: (a -> b) -> Rec0 a x -> Rec0 b x
  gfmap f (K1 a) = K1 (f a)

instance {-# INCOHERENT #-} GFunctor (Rec0 x) (Rec0 x) a b where
  gfmap :: (a -> b) -> Rec0 x1 x2 -> Rec0 x1 x2
  gfmap _ = id

class Functor' p where
  fmap' :: (a -> b) -> p a -> p b
  default fmap' ::
    ( Generic (p a),
      Generic (p b),
      GFunctor (Rep (p a)) (Rep (p b)) a b
    ) =>
    (a -> b) ->
    p a ->
    p b
  fmap' f = to . gfmap f . from

newtype Id a = Id a
  deriving (Generic, Show)

deriving instance Functor' Id
