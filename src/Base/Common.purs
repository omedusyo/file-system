module Base.Common where

import Prelude

data Tuple2 a = Tuple2 a a
data Tuple4 a = Tuple4 a a a a
data Tuple8 a = Tuple8 a a a a a a a a

derive instance tuple2Eq :: Eq a => Eq (Tuple2 a)
derive instance tuple4Eq :: Eq a => Eq (Tuple4 a)
derive instance tuple8Eq :: Eq a => Eq (Tuple8 a)

instance showTuple8 :: Show a => Show (Tuple8 a) where
  show (Tuple8 x0 x1 x2 x3 x4 x5 x6 x7) =
    "(Tuple8 " <> show x0 <> " " <> show x1 <> " " <> show x2 <> " " <> show x3 <> " " <> show x4 <> " " <> show x5 <> " " <> show x6 <> " " <> show x7 <> ")"

repeat8 :: forall a . a -> Tuple8 a
repeat8 x = Tuple8 x x x x x x x x

concat4To4 :: forall a . Tuple4 a -> Tuple4 a -> Tuple8 a
concat4To4 (Tuple4 x0 x1 x2 x3) (Tuple4 y0 y1 y2 y3) =
  Tuple8 x0 x1 x2 x3 y0 y1 y2 y3

data Tuple2Index a =
    T2I0 a
  | T2I1 a

data Tuple4Index a =
    T4I0 a
  | T4I1 a
  | T4I2 a
  | T4I3 a

data Tuple8Index a =
    I0 a
  | I1 a
  | I2 a
  | I3 a
  | I4 a
  | I5 a
  | I6 a
  | I7 a


update8At :: forall a indexType . Tuple8Index indexType -> (indexType -> a -> a) -> Tuple8 a -> Tuple8 a
update8At i f (Tuple8 x0 x1 x2 x3 x4 x5 x6 x7) =
  case i of
    I0 j -> Tuple8 (f j x0)     x1      x2      x3      x4      x5      x6      x7
    I1 j -> Tuple8      x0 (f j x1)     x2      x3      x4      x5      x6      x7
    I2 j -> Tuple8      x0      x1 (f j x2)     x3      x4      x5      x6      x7
    I3 j -> Tuple8      x0      x1      x2 (f j x3)     x4      x5      x6      x7
    I4 j -> Tuple8      x0      x1      x2      x3 (f j x4)     x5      x6      x7
    I5 j -> Tuple8      x0      x1      x2      x3      x4 (f j x5)     x6      x7
    I6 j -> Tuple8      x0      x1      x2      x3      x4      x5 (f j x6)     x7
    I7 j -> Tuple8      x0      x1      x2      x3      x4      x5      x6 (f j x7)

get8At :: forall a b indexType . Tuple8Index indexType -> (indexType -> a -> b) -> Tuple8 a -> b
get8At i f (Tuple8 x0 x1 x2 x3 x4 x5 x6 x7) =
  case i of
    I0 j -> f j x0
    I1 j -> f j x1
    I2 j -> f j x2
    I3 j -> f j x3
    I4 j -> f j x4
    I5 j -> f j x5
    I6 j -> f j x6
    I7 j -> f j x7
