module Util (cmp, integerToBool, boolToInteger) where

--
-- Given a boolean operation and two elements, returns a 1 if the answer
-- is True, and 0 if the answer is False.
--
cmp :: (a -> a -> Bool) -> a -> a -> Integer
cmp pred a b = boolToInteger $ pred a b

integerToBool :: Integer -> Bool
integerToBool 0 = False
integerToBool _ = True

boolToInteger :: Bool -> Integer
boolToInteger False = 0
boolToInteger True  = 1
