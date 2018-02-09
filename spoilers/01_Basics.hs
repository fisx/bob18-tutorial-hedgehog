{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Basics where

import qualified Control.Exception
import           Data.List
import           Debug.Trace
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


go :: IO ()
go = do
  _ <- checkSequential $$(discover)
  pure ()


-- * a simple example

prop_const :: Property
prop_const = property $ True === True

  -- change to @True === False@
  -- go through error report
  -- 'succeess', 'failure', 'assert' (see Hedgehog.Property on hackage for more)
  -- diff feature: change to @[1..10] === [2..11]@  -- TODO: this does not appear to work?


prop_sort1 :: Property
prop_sort1 = property $ do
  xs :: [Int] <- pure [1, 2, 3]
  length (sort xs) === length xs

  -- 'length' could be replaced by 'sum'.  add that.

prop_sort2 :: Property
prop_sort2 = property $ do
  xs :: [Int] <- forAll $ Gen.list
                   (Range.linear 0 100)
                   (Gen.int (Range.linear (-100) 100))
  length (sort xs) === length xs

  -- whatch xs with 'traceShow'
  -- try Range.exponential


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

existsPairSumOracle :: [Integer] -> Integer -> Bool
existsPairSumOracle xs x = x `elem` [ a + b | a <- xs, b <- xs \\ [a] ]

prop_existsPairSum :: Property
prop_existsPairSum = property $ do
  let genInt = Gen.integral (Range.linear (-50) 50)
  xs <- forAll $ Gen.list (Range.linear 0 100) genInt
  x  <- forAll genInt
  existsPairSum xs x === existsPairSumOracle xs x

  -- start with maxInt = 50.
  -- make 'otherwise' case in 'f' return 'False'.  error!  good!
  -- let genInt = fromIntegral <$> Gen.int Range.linearBounded
  -- use trace to find out what is going wrong.  then use annotate for the same thing.
  -- solutions:
  -- Gen.constant (sum $ take 2 xs)
  -- Gen.element [sum $ take 2 xs, sum xs + 1]
  -- Gen.choice [pure . sum $ take 2 xs, pure $ sum xs + 1, genInt]

  -- generators are things that can be added into tests via 'forAll'.  then
  -- generate values.  qc: arbitrary.

  -- if in qc you sometimes have to write functions 'arbitrarySomething', you
  -- write a hedgehog generator.

  -- example for usefulness of do notation: gen a list that always starts with
  -- '0'.  translate to <$>, <*>.

  -- think of ranges as pairs of integers.  there is 'Size', but its an
  -- uninteresting complication.
  -- https://github.com/hedgehogqa/haskell-hedgehog/issues/38#issuecomment-293424765
