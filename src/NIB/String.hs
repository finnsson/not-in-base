-- {-# OPTIONS_GHC -fglasgow-exts -,  
--    
--  #-}
{-# LANGUAGE UndecidableInstances,  OverlappingInstances, FlexibleInstances, TypeSynonymInstances
 #-}
{-|
  Contains useful generic functions not found elsewhere.


-}
module NIB.String where

import Data.Char

import NIB.List
import NIB.Pointfree


-- \ String functions

-- \ ToString type class as found on stackoverflow (Porges answer at
-- http://stackoverflow.com/questions/968198/haskell-show-screwed-up)
-- Expanded with instance ToString Char.
class ToString a where
    toString :: a -> String

instance ToString String where
    toString = id

instance ToString Char where
    toString c = [c]

instance Show a => ToString a where
    toString = show



-- | Remove all line breaks in a string
--
-- > "testtest" == removeBreak "test\n\rtest\r"
removeBreak :: String -> String
removeBreak = filter ((/= '\r') &&* (/= '\n'))

-- | Convert first character in String to lower.
--
-- > lowerFirst "Foo" == "foo"
-- > lowerFirst "BaR" == "baR"
-- > lowerFirst "g0O" == "g0O".'
lowerFirst :: String -> String
lowerFirst = convertFirst toLower

-- | Convert first character in String to upper.
--
-- > upperFirst "foo" == "Foo"
-- > upperFirst "bAr" == "BAr"
-- > upperFirst "G0O" == "G0O".'
upperFirst :: String -> String
upperFirst = convertFirst toUpper

-- | Convert every space (' ') in a string to a blank ('_') instead. 
--
-- > spaceToBlank " " == "_"
-- > spaceToBlank " foo  " == "_foo__"
-- > spaceToBlank "b a r" == "b_a_r"
spaceToBlank :: String -> String
spaceToBlank "" = ""
spaceToBlank (x:xs) = (if x == ' ' then '_' else x) : spaceToBlank xs



-- | Trims whitespace from the beginning or end.
--
-- > trimWs "  foo  " == "foo"
trimWs :: String -> String
trimWs = trim (==' ')


