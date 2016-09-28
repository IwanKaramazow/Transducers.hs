{-# LANGUAGE RankNTypes #-}

module Transducers where

-- r = reduced
type ReducingFn r a = r -> a -> r

type Transducer a b = forall r . ReducingFn r a -> ReducingFn r b

xmap :: (a -> b) -> Transducer b a
xmap f xf r a = xf r $ f a

xfilter :: (a -> Bool) -> Transducer a a
xfilter predicate xf r a = if predicate a then xf r a else r

-- left folds dominate in Clojure
-- xform f init coll
transduce :: Foldable t => Transducer a b -> ReducingFn r a -> r -> t b -> r
transduce xform f = foldl (xform f)

-- end of the library :D

-- convencience...
class Conjable f where
  empty :: f a
  conj :: ReducingFn (f a) a


instance Conjable [] where
  empty = []
  conj xs x = xs ++ [x]


xlist :: (Foldable t, Conjable t) => Transducer b a -> t a -> t b
xlist xform = transduce xform conj empty
