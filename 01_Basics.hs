{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Basics where

import           Data.List
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


prop_const :: Property
prop_const = property $ True === True

iAmReady :: IO ()
iAmReady = do
  _ <- checkParallel $$(discover)
  pure ()







-- * oracles

-- if you find an algorithm is too slow, and you think of a faster one, you can
-- keep the old one in your test suite.  sometimes, it's easy to start with a
-- really slow algorithm that is really easy to understand and reason about, and
-- move that to the test suite right away.  this is your self-implemented
-- oracle.

-- | Decide whether two 'Int's with a given sum can be found in an 'Int' list.
-- <https://www.youtube.com/watch?v=XKu_SEDAykw>
--
-- @
-- existsPairSum [1, 2, 3, 9] 8 == False
-- existsPairSum [1, 2, 4, 4] 8 == True
-- existsPairSum [1, 2, 4, 12, 13, 100] 104 == True
-- existsPairSum [0, 0, 2] 2 == True
-- @
existsPairSum :: [Integer] -> Integer -> Bool
existsPairSum xs goal = f xs' (reverse xs')
  where
    xs' = zip [(0 :: Integer)..] $ sort xs

    f [] _ = False
    f _ [] = error "impossible."
    f ((i, _):_) ((i', _):_) | i >= i' = False
    f xs_@((_, x):xs_') ys_@((_, y):ys_') = case x + y of
      shot | shot == goal -> True
           | shot < goal  -> f xs_' ys_
           | otherwise    -> f xs_ ys_'
