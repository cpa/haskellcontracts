{-# LANGUAGE DeriveDataTypeable, 
    DeriveFunctor, 
    NoMonomorphismRestriction, 
    RankNTypes, 
    ImpredicativeTypes,
    ScopedTypeVariables #-}
module Generics(gfmap) where
import Data.Generics

-- | Apply a generic transform 'f' at the head of each "maximal"
-- subterm of 'x' matching the predicate 'q'.  NB: the transform is
-- not 'f' is *not* applied recursively in recursive types matching
-- 'q'.  That's the point.  E.g. consider
--
--   everywhere (mkT (reverse::[Int]->[Int])) ([1,2,3]::[Int],[4,5,6]::[Int])
--   == ([2,3,1],[5,6,4])
--
-- and not
--
--   == ([3,2,1],[6,5,4])
--
-- which you might want.  On the other hand, 'deepOnce' gives the
-- latter behaviour:
--
--   deepOnce (mkQ False (const True::[Int]->Bool))
--            (mkT (reverse::[Int]->[Int]))
--            (([1,2,3],[4,5,6])::([Int],[Int]))
--   == ([3,2,1],[6,5,4])
--
-- of course, all those annotations are a little painful, so see
-- 'gfmap'.
deepOnce :: GenericQ Bool -> GenericT -> GenericT
deepOnce q f x
    | q x       = f x
    | otherwise = gmapT (deepOnce q f) x

-- | Deeply apply a transformation 't :: a -> a' to the "maximal"
-- subterms of type 'a'.  A more convenient way to use 'deepOnce'.
--
-- This can replace all the two-level types with 'type T = MetaT E'
-- and 'data MetaT e = C e | ... deriving Functor' stuff.
gfmap :: forall a. Typeable a => (a -> a) -> GenericT
gfmap t = deepOnce q (mkT t) where
  -- Use 'ScopedTypeVariables' to make 'a' here refer to 'a' in sig
  -- for 'gfmap'.  An alternative that doesn't need the extension is
  --
  --   qForT :: (a -> a) -> (a -> Bool)
  --   qForT _ = const True
  --   q = mkQ False $ qForT t
  --
  -- where we use the ignored 't' argument to 'qForT' to make the 'a's
  -- the same.
  q = mkQ False (const True :: a -> Bool)



-- Generic programming examples
-- ----------------------------
data Tree a = Leaf a
            | Branch (Tree a) (Tree a)
            deriving (Show,Eq,Data,Typeable,Functor)

type TreeI = Tree Int

-- no sig doesn't work and doesn't work
--f :: (Typeable a, Num a) => Tree a -> Tree a
--f :: (Typable a, Data a, Num a) => Tree a -> Tree a
-- but
--f :: Tree Int -> Tree Int
-- works

f2 :: TreeI -> TreeI
f2 = fmap (*2)

f3 :: Int -> Int
f3 = (*2)

f1 :: TreeI -> TreeI
f1 (Leaf a) = Leaf (a*2)
f1 g = g

eg1 :: [TreeI]
-- no, seem to need 'Data a' to use 'everywhere', i.e. even sub terms in other types must be 'Data'.
-- eg1 :: Num a => Tree a
-- eg1 :: (Data a, Num a) => Tree a
eg1 = [everywhere (mkT f1) t, everywhere (mkT f2) t, everywhere (mkT f3) t]
  -- map (flip everywhere t) $ map mkT fs
 where
  t = (Branch (Leaf 3) (Branch (Leaf 5) (Leaf 7)))::TreeI
  f' :: GenericT -- Data a => a -> a
  f' = mkT (f1::TreeI -> TreeI)
  fs = [f1,f2]
--  fs' = [mkT f1,mkT f2]::[GenericT]

-- fmap fail: want to apply an operation at all the "leaves", but not
-- recursively in the leaves.  i.e., something like 'onceDeep' is
-- wanted.
type TreeLI = Tree LI
type LI = [Int]

revLI :: LI -> LI
revLI = reverse
revLIT = mkT revLI

t2 = Branch (Leaf [1,2,3]) (Branch (Leaf [4,5,6]) (Leaf [7,8,9]))

--tRev :: TreeLI -> TreeLI
tRev = everywhere revLIT

eg2 :: TreeLI
eg2 = tRev t2

isLI :: LI -> Bool
isLI = const True
isLIQ = mkQ False isLI

eg3 :: TreeLI
eg3 = deepOnce isLIQ revLIT t2

eg4 :: TreeLI
eg4 = gfmap revLI t2

eg5 = fmap reverse t2

type RoseTLI = Rose TreeLI
data Rose a
   = Rose a [Rose a]
   deriving (Show,Eq,Data,Typeable,Functor)

r1 = Rose t2 [r2]
r2 = Rose t2 []

eg6 :: RoseTLI
eg6 = gfmap revLI r1

eg7 = (fmap . fmap) reverse r1

type CarefulRTLI = Careful RoseTLI RoseTLI
data Careful a b
   = A a
   | B b
   | AB a b -- how to only handle 'a' or 'b', but not both, when
            -- they're the same?
   | C [Careful a b]
   deriving (Show,Eq,Data,Typeable,Functor)

c1 = C [B r2, A r2]

-- the 'fmap' only acts on the 'b's.
eg8 = (fmap . fmap . fmap) reverse c1

eg9,eg10,eg11 :: CarefulRTLI
eg9 = gfmap revLI c1

-- can't handle the 'AB' case properly.
eg10 = deepOnce (mkQ False isA) (gfmap revLI) c1 where
  isA :: CarefulRTLI -> Bool
  isA (A _) = True
  isA _     = False
eg11 = deepOnce (mkQ False isB) (gfmap revLI) c1 where
  isB :: CarefulRTLI -> Bool
  isB (B _) = True
  isB _     = False

c2 :: Careful RoseTLI TreeLI
c2 = C [A r2,B t2]
eg12,eg13,eg14 :: Careful RoseTLI TreeLI
-- all lists
eg12 = gfmap revLI c2
-- lists in 'B': can't figure this one out :P
eg13 = gfmap t c2 where
  t :: TreeLI -> TreeLI
  t = mkT ((id::RoseTLI -> RoseTLI) `extT` revLI)
-- lists in 'A'
eg14 = gfmap (gfmap revLI :: RoseTLI -> RoseTLI) c2
