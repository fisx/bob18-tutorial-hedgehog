{-# LANGUAGE TemplateHaskell #-}

module Basics where

import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range

prop_const :: Property
prop_const = property $ 1 === 1

iAmReady :: IO ()
iAmReady = do
  _ <- checkParallel $$(discover)
  pure ()
