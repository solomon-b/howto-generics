module Data.Trifunctor where

--------------------------------------------------------------------------------

import GHC.Generics

--------------------------------------------------------------------------------

class GTrifunctor s t a b c d e f where
  gtrimap :: (a -> b) -> (c -> d) -> (e -> f) -> s x -> t x

instance GTrifunctor s t a b c d e f => GTrifunctor (M1 k m s) (M1 k m t) a b c d e f where
  gtrimap ::
    GTrifunctor s t a b c d e f =>
    (a -> b) ->
    (c -> d) ->
    (e -> f) ->
    M1 k m s x ->
    M1 k m t x
  gtrimap f g h = M1 . gtrimap f g h . unM1

instance
  ( GTrifunctor x x' a b c d e f,
    GTrifunctor y y' a b c d e f,
    GTrifunctor z z' a b c d e f
  ) =>
  GTrifunctor (x :+: y :+: z) (x' :+: y' :+: z') a b c d e f
  where
  gtrimap ::
    (GTrifunctor x x' a b c d e f, GTrifunctor y y' a b c d e f, GTrifunctor z z' a b c d e f) =>
    (a -> b) ->
    (c -> d) ->
    (e -> f) ->
    (:+:) x (y :+: z) x1 ->
    (:+:) x' (y' :+: z') x1
  gtrimap f g h (L1 x) = L1 (gtrimap f g h x)
  gtrimap f g h (R1 (L1 y)) = R1 (L1 (gtrimap f g h y))
  gtrimap f g h (R1 (R1 z)) = R1 (R1 (gtrimap f g h z))

instance
  ( GTrifunctor x x' a b c d e f,
    GTrifunctor y y' a b c d e f,
    GTrifunctor z z' a b c d e f
  ) =>
  GTrifunctor (x :*: y :*: z) (x' :*: y' :*: z') a b c d e f
  where
  gtrimap ::
    (GTrifunctor x x' a b c d e f, GTrifunctor y y' a b c d e f, GTrifunctor z z' a b c d e f) =>
    (a -> b) ->
    (c -> d) ->
    (e -> f) ->
    (:*:) x (y :*: z) x1 ->
    (:*:) x' (y' :*: z') x1
  gtrimap f g h (x :*: y :*: z) = gtrimap f g h x :*: gtrimap f g h y :*: gtrimap f g h z

instance GTrifunctor U1 U1 a b c d e f where
  gtrimap :: (a -> b) -> (c -> d) -> (e -> f) -> U1 x -> U1 x
  gtrimap _ _ _ = id

instance {-# INCOHERENT #-} GTrifunctor (Rec0 a) (Rec0 b) a b c d e f where
  gtrimap :: (a -> b) -> (c -> d) -> (e -> f) -> Rec0 a x -> Rec0 b x
  gtrimap f _ _ (K1 a) = K1 (f a)

instance {-# INCOHERENT #-} GTrifunctor (Rec0 c) (Rec0 d) a b c d e f where
  gtrimap :: (a -> b) -> (c -> d) -> (e -> f) -> Rec0 c x -> Rec0 d x
  gtrimap _ g _ (K1 a) = K1 (g a)

instance {-# INCOHERENT #-} GTrifunctor (Rec0 e) (Rec0 f) a b c d e f where
  gtrimap :: (a -> b) -> (c -> d) -> (e -> f) -> Rec0 e x -> Rec0 f x
  gtrimap _ _ h (K1 a) = K1 (h a)

instance {-# INCOHERENT #-} GTrifunctor (Rec0 x) (Rec0 x) a b c d e f where
  gtrimap :: (a -> b) -> (c -> d) -> (e -> f) -> Rec0 x1 x2 -> Rec0 x1 x2
  gtrimap _ _ _ = id

class Trifunctor p where
  trimap :: (a -> b) -> (c -> d) -> (e -> f) -> p a c e -> p b d f
  default trimap ::
    ( Generic (p a c e),
      Generic (p b d f),
      GTrifunctor (Rep (p a c e)) (Rep (p b d f)) a b c d e f
    ) =>
    (a -> b) ->
    (c -> d) ->
    (e -> f) ->
    p a c e ->
    p b d f
  trimap f g h = to . gtrimap f g h . from

deriving instance Trifunctor (,,)
