{-# LANGUAGE UndecidableInstances,  OverlappingInstances, FlexibleInstances, TypeSynonymInstances
 #-}

module NIB.Pointfree where

import Data.Char
import Data.Maybe
import Data.List
import Control.Monad

-- \ Tuple functions

fst3 :: (a,b,c) -> a
fst3 (a,b,c) = a

snd3 :: (a,b,c) -> b
snd3 (a,b,c) = b

trd3 :: (a,b,c) -> c
trd3 (a,b,c) = c
trd (a,b,c) = c


-- | Lambdifies a function. See '(||*)' and '(&&*)' for uses of 'lambdify'.
-- | Used in order to make operators capable of operating on functions that later on
-- | are supplied some value that all functions operate on.
--
-- > (+*) = lambdify (+)
-- > fourTwo = (*4) +* (*2)
-- > 42 == fourTwo 7
lambdify :: (x -> y -> z) -> (t -> x) -> (t -> y) -> t -> z
lambdify f a b x = f (a x) (b x)

-- | Lambdifies '(||)'.
--
-- > isBlankOrCommaChecker = (==' ') ||* (==',')
-- > isBlankOrComma = isBlankOrCommaChecker 'j'
(||*) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(||*) = lambdify (||)

-- | Lambdifies '(&&)'.
--
-- > isInRangeChecker = (>9) &&* (<30)
-- > isInRange = isInRangeChecker 17
(&&*) :: (a -> Bool) -> (a -> Bool) -> a -> Bool
(&&*) = lambdify (&&)

-- | 2-point-free operator. Similar to '.', but where
-- | the second function takes two (2) arguments instead of one (1).
--
-- > multAndSquare (^2) .^.. (*)
-- > 36 == multAndSqare 2 3
(^..) :: (c -> d) -> (a -> b -> c) -> a -> b-> d
(f ^.. g) a = f . g a

-- | 3-point-free operator. See '(^..)'.
(^...) :: (d -> e) -> (a -> b -> c -> d) -> a -> b -> c -> e
(f ^...g ) a b = f . g a b

-- | Split a 2-tuple 'x' into a 2-stack and pass it to 'f'.
-- | The same as uncurry.
(..%) :: (a -> b -> c) -> (a,b) -> c
(..%) = uncurry 

(..%..) :: (c->d->e) -> (a->b->(c,d)) -> a -> b -> e
(f ..%.. g) a b = f ..% g a b

-- | Split a 3-tuple 'x' into a 3-stack and pass it to 'f'.
(...%) :: (a -> b -> c -> d) -> (a,b,c) -> d
(...%) f x = f (fst3 x) (snd3 x) (trd3 x)


-- | Pipes a monadic return through a non-monadic transformation function.
-- | liftM with arguments flipped.
--
-- > readIO >>* toUpper
(>>*) :: Monad m => m a -> (a -> b) -> m b
(>>*) v f = liftM f v -- v >>= (return . f)

-- > f = ((+) 2, (*) 3)
-- > x = 7
-- > r = f ..@ x
-- 
-- | gives `(9, 21)`, i.e. `(2 + 9, 3 * 7)`.
(..@) :: (a -> b, a -> c) -> a -> (b,c) 
f ..@ x = (fst f x, snd f x)

-- | Same as `..@`, but with a 3-tuple. 
(...@) :: (a -> b, a -> c, a -> d) -> a -> (b,c,d)
f ...@ x = (fst3 f x, snd3 f x, trd3 f x)
