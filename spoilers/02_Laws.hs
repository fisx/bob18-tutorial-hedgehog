{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | polymorphic types and type classes often have laws that are expected to
-- hold for all their instances.  these laws are nothing *but* properties, so
-- let's test some of them!
module Laws where

import           Control.Lens
import           Data.Monoid
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range


go :: IO ()
go = do
  _ <- checkSequential $$(discover)
  pure ()


-- * lens laws

-- | special case: @_1 = Lens' (Bool, Int) Bool@
--
-- intuitively (and not very accurately), a lens is a pair of getter, setter for
-- some type.  for what we need in the following, you can read the following as
-- language in itself, without understanding how exactly it works in the Haskell
-- semanics:
--
-- >>> _1 :: Lens (a, b) a
-- >>> *Laws> (1, False) ^. _1
-- >>> 1
-- >>> *Laws> (False, 1) ^. _1
-- >>> False
-- >>> *Laws> (False, 1) & _1 .~ True
-- >>> (True,1)
--
-- all lenses have to satisfy a number of properties (laws)
-- <https://hackage.haskell.org/package/lens-4.16/docs/Control-Lens-Type.html#t:Lens>
--
-- - You get back what you put in
-- - Putting back what you got doesn't change anything
-- - Setting twice is the same as setting once
--
prop_Lens1 :: Property
prop_Lens1 = property $ do
  let lns :: Lens' (Bool, Int) Bool
      lns = _1

  s :: (Bool, Int) <- forAll $ (,) <$> Gen.bool <*> Gen.int (Range.linear 1 10)
  v :: Bool        <- forAll Gen.bool

  ((s & lns .~ v) ^. lns)    ===  v
  (s & lns .~ (s ^. lns))    ===  s
  (s & lns .~ v & lns .~ v)  ===  (s & lns .~ v)


-- | general form: can be exported from a package e.g. lens-laws.  then anybody
-- who writes a new lens for their own data type can use it for an
-- almost-for-free soundness check.
--
lensLaws :: (Show s, Show v, Eq s, Eq v)
         => Lens' s v -> Gen s -> Gen v -> Property
lensLaws lns sgen vgen = property $ do
  s <- forAll sgen
  v <- forAll vgen

  ((s & lns .~ v) ^. lns)    ===  v
  (s & lns .~ (s ^. lns))    ===  s
  (s & lns .~ v & lns .~ v)  ===  (s & lns .~ v)


-- | '_1' instance.
prop_Lens2 :: Property
prop_Lens2 = lensLaws _1
  ((,) <$> Gen.bool <*> Gen.int (Range.linear 1 10))
  Gen.bool

  -- replace _1 with undefined and read the error.


-- * monoid laws

-- | this is almost unchanged cut & paste from
-- <https://hackage.haskell.org/package/base/docs/Data-Monoid.html>!
--
monoidLaws :: (Monoid m, Show m, Eq m) => Gen m -> Property
monoidLaws mgen = property $ do
  x <- forAll mgen
  y <- forAll mgen
  z <- forAll mgen
  xs <- forAll $ Gen.list (Range.exponential 1 10) mgen

  mappend mempty x === x
  mappend x mempty === x
  mappend x (mappend y z) === mappend (mappend x y) z
  mconcat xs === foldr mappend mempty xs


prop_MonoidIntList :: Property
prop_MonoidIntList = monoidLaws $ Gen.list (Range.linear 1 100) (Gen.int Range.linearBounded)


-- | another example
--
-- >>> *Laws>  Last Nothing <> Last Nothing
-- >>> Last (Nothing)
-- >>> *Laws> Last Nothing <> Last (Just True)
-- >>> Last (Just True)
-- >>> *Laws>  Last (Just True) <> Last Nothing
-- >>> Last (Just True)
-- >>> *Laws>  Last (Just True) <> Last Nothing <> Last (Just False) <> Last Nothing
-- >>> Last (Just False)
--
prop_MonoidLast :: Property
prop_MonoidLast = monoidLaws $ Last <$> Gen.choice [pure Nothing, Just <$> Gen.bool]
