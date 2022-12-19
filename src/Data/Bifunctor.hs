module Data.Bifunctor where

--------------------------------------------------------------------------------

import GHC.Generics

--------------------------------------------------------------------------------

class GBifunctor s t a b c d where
  gbimap :: (a -> b) -> (c -> d) -> s x -> t x

instance GBifunctor s t a b c d => GBifunctor (M1 k m s) (M1 k m t) a b c d where
  gbimap ::
    GBifunctor s t a b c d =>
    (a -> b) ->
    (c -> d) ->
    M1 k m s x ->
    M1 k m t x
  gbimap f g = M1 . gbimap f g . unM1

instance
  ( GBifunctor l l' a b c d,
    GBifunctor r r' a b c d
  ) =>
  GBifunctor (l :+: r) (l' :+: r') a b c d
  where
  gbimap ::
    (GBifunctor l l' a b c d, GBifunctor r r' a b c d) =>
    (a -> b) ->
    (c -> d) ->
    (:+:) l r x ->
    (:+:) l' r' x
  gbimap f g (L1 l) = L1 (gbimap f g l)
  gbimap f g (R1 r) = R1 (gbimap f g r)

instance
  ( GBifunctor l l' a b c d,
    GBifunctor r r' a b c d
  ) =>
  GBifunctor (l :*: r) (l' :*: r') a b c d
  where
  gbimap ::
    (GBifunctor l l' a b c d, GBifunctor r r' a b c d) =>
    (a -> b) ->
    (c -> d) ->
    (:*:) l r x ->
    (:*:) l' r' x
  gbimap f g (l :*: r) = gbimap f g l :*: gbimap f g r

instance GBifunctor U1 U1 a b c d where
  gbimap :: (a -> b) -> (c -> d) -> U1 x -> U1 x
  gbimap _ _ = id

instance {-# INCOHERENT #-} GBifunctor (Rec0 a) (Rec0 b) a b c d where
  gbimap :: (a -> b) -> (c -> d) -> Rec0 a x -> Rec0 b x
  gbimap f _ (K1 a) = K1 (f a)

instance {-# INCOHERENT #-} GBifunctor (Rec0 c) (Rec0 d) a b c d where
  gbimap :: (a -> b) -> (c -> d) -> Rec0 c x -> Rec0 d x
  gbimap _ g (K1 a) = K1 (g a)

instance {-# INCOHERENT #-} GBifunctor (Rec0 x) (Rec0 x) a b c d where
  gbimap :: (a -> b) -> (c -> d) -> Rec0 x1 x2 -> Rec0 x1 x2
  gbimap _ _ = id

class Bifunctor p where
  bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
  default bimap ::
    ( Generic (p a c),
      Generic (p b d),
      GBifunctor (Rep (p a c)) (Rep (p b d)) a b c d
    ) =>
    (a -> b) ->
    (c -> d) ->
    p a c ->
    p b d
  bimap f g = to . gbimap f g . from

deriving instance Bifunctor Either

deriving instance Bifunctor (,)
