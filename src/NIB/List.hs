{-# LANGUAGE UndecidableInstances,  OverlappingInstances, FlexibleInstances, TypeSynonymInstances
 #-}

module NIB.List where

import Data.Char
import Data.Maybe
import Data.List
import Control.Monad


-- | Splits a list @x@ of @a@ into a list of lists of @a@ at every @c@.
--
-- >  "splitBy "foo,bar" "',' == ["foo","bar"] ' 
splitBy :: (Eq a) => a -> [a] -> [[a]]
splitBy _ [] = [[]]
splitBy c x  = if fst p == [] then [snd p] else fst p : splitBy c ( snd p )
               where p = break (== c) x

-- | Trims every element satisfying @c@ from the beginning or end of the list.
--
-- > trim (==' ') "  foo   " == "foo"
trim :: (a -> Bool) -> [a] -> [a]
trim c = reverse . dropWhile c . reverse . dropWhile c

-- | Convert first element in list
--
-- > convertFirst (toUpper) "fO0" == "FO0"
convertFirst :: (a -> a) -> [a] -> [a]
convertFirst _ [] = []
convertFirst f (x:xs) = f x:xs


