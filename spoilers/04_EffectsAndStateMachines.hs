{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | A minimal example for testing with state machines.
--
-- This is a variation of
-- <https://github.com/hedgehogqa/haskell-hedgehog/blob/master/hedgehog-example/src/Test/Example/References.hs>
-- If you are done / stuck with this please go check out the original.
--
-- Further reading:
-- <http://teh.id.au/posts/2017/07/15/state-machine-testing/index.html>,
-- <http://clrnd.com.ar/posts/2017-04-21-the-water-jug-problem-in-hedgehog.html>,
-- <https://github.com/advancedtelematic/quickcheck-state-machine#quickcheck-state-machine>
module EffectsAndStateMachines where

import Hedgehog
import Hedgehog.Gen as Gen
import Hedgehog.Range as Range
import Control.Monad.IO.Class
import Control.Concurrent.MVar as MVar
import Debug.Trace


-- * the system we want to test

mnew :: IO (MVar Int)
mnew = MVar.newMVar 0

mread :: MVar Int -> IO Int
mread = MVar.readMVar

mwrite :: MVar Int -> Int -> IO ()
mwrite mvar i = MVar.modifyMVar_ mvar $ \_ -> pure i

minc :: MVar Int -> IO ()
minc mvar = do
  i <- mread mvar
  mwrite mvar (i - 1)


-- * state model

type V = Var (Opaque (MVar Int))

newtype ModelState (v :: * -> *) = Store [(V v, Int)]
  deriving (Eq, Show)

initialModelState :: ModelState v
initialModelState = Store mempty


-- * actions

data MNew (v :: * -> *) = MNew
  deriving (Eq, Show)

data MRead (v :: * -> *) = MRead (V v)
  deriving (Eq, Show)

data MWrite (v :: * -> *) = MWrite (V v) Int
  deriving (Eq, Show)

data MInc (v :: * -> *) = MInc (V v)
  deriving (Eq, Show)


instance HTraversable MNew where
  htraverse _ MNew = pure MNew

instance HTraversable MRead where
  htraverse f (MRead var) = MRead <$> htraverse f var

instance HTraversable MWrite where
  htraverse f (MWrite var i) = MWrite <$> htraverse f var <*> pure i

instance HTraversable MInc where
  htraverse f (MInc var) = MInc <$> htraverse f var


s_mnew :: (MonadGen n, MonadTest m, MonadIO m) => Command n m ModelState
s_mnew = Command gen ex callbacks
  where
    gen _ = Just $ pure MNew

    ex MNew = Opaque <$> liftIO mnew

    callbacks :: [Callback MNew (Opaque (MVar Int)) ModelState]
    callbacks =
      [ Update $ \(Store heap) MNew o -> Store $ (o, 0) : heap
      ]

s_mread :: (MonadGen n, MonadTest m, MonadIO m) => Command n m ModelState
s_mread = Command gen ex callbacks
  where
    gen (Store []) = Nothing
    gen (Store heap@(_:_)) = Just $ MRead <$> Gen.element (fst <$> heap)

    ex (MRead mvar) = liftIO $ mread (opaque mvar)

    -- hedgehog issue coming up (request for help, not bug report yet).  for
    -- now, just leave this to 'False', and the tutorial will be fine.
    callbacks :: [Callback MRead Int ModelState]
    callbacks =
      [ Ensure $ \(Store before) (Store after) (MRead mvar) o -> do
          let t heap = case Prelude.filter ((== mvar) . fst) heap of
                [(_, model)] -> o === model

                  -- TODO: parallel testing fails.  some experiments:
                  --
                  -- o === model
                  -- => loops in parallel test
                  --
                  -- if o == model then pure () else error "***"
                  -- => throws error in the 'else' branch
                  --
                  -- pure ()
                  -- => works fine (but doesn't really check the invariant we intended)
                  --
                  -- length (show (heap, o, model, o == model)) `Prelude.seq` pure ()
                  -- => works fine (as above)

                bad -> annotateShow bad >> failure
          t before
          t after
      ]

s_mwrite :: (MonadGen n, MonadTest m, MonadIO m) => Command n m ModelState
s_mwrite = Command gen ex callbacks
  where
    gen (Store []) = Nothing
    gen (Store heap@(_:_)) = Just $
      MWrite <$> Gen.element (fst <$> heap) <*> Gen.int (Range.linear 1 100)

    ex (MWrite mvar i) = liftIO $ mwrite (opaque mvar) i

    callbacks :: [Callback MWrite () ModelState]
    callbacks =
      [ Update $ \(Store heap) (MWrite mvar i) _ ->
          Store $ (mvar, i) : Prelude.filter ((/= mvar) . fst) heap

      , Ensure $ \(Store _before) (Store after) (MWrite mvar i) () -> do
          let t heap = case Prelude.filter ((== mvar) . fst) heap of
                [(_, mdl)] -> i === mdl
                bad -> annotateShow bad >> failure
          t after
      ]

    -- (there is also 'Require' callback constructor: if the system would look
    -- up an MVar in a Map, we could check that it's available.)

s_minc :: (MonadGen n, MonadTest m, MonadIO m) => Command n m ModelState
s_minc = Command gen ex callbacks
  where
    gen (Store []) = Nothing
    gen (Store heap@(_:_)) = Just $ MInc <$> Gen.element (fst <$> heap)

    ex (MInc mvar) = liftIO $ minc (opaque mvar)

    callbacks :: [Callback MInc () ModelState]
    callbacks =
      [ Update $ \(Store heap) (MInc mvar) _ ->
          Store $ (\(mvar', i) -> (mvar', if mvar' == mvar then i + 1 else i)) <$> heap
      ]

    -- post-conditions with 'Ensure' do not support access to the handles /
    -- references into the system under test: its type is 'Test', not 'TestT m',
    -- so we cannot replace 'Identity' with 'IO'.  hence there is no
    -- straight-forward way to test that model and actual reference contain the
    -- same value after the increment.
    -- <https://github.com/hedgehogqa/haskell-hedgehog/issues/160>


-- * properties

prop_mvar :: Property
prop_mvar = property $ do
  actions <- forAll $ Gen.sequential (Range.linear 1 100) initialModelState
    [ s_mnew
    , s_mread
    , s_mwrite
    , s_minc
    ]
  executeSequential initialModelState actions

{-
prop_mvar' :: Property
prop_mvar' = withTests 100 . withRetries 1 . property $ do
  actions <- forAll $ Gen.parallel (Range.linear 1 100) (Range.linear 1 100) initialModelState
    [ s_mnew
    , s_mread
    , s_mwrite
    , s_minc
    ]
  test $ executeParallel initialModelState actions
-}


go :: IO Bool
go =
  checkParallel $$(discover)


-- fix inc.
-- fix race condition.
