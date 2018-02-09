{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | Great blog article explaining and advocating round-trip property tests:
-- <http://teh.id.au/posts/2017/06/07/round-trip-property/>
module Tripping where

import           Control.Applicative ((<|>))
import           Data.Monoid ((<>))
import qualified Data.Text as T
import qualified Data.Text.Lazy as TL
import qualified Data.Text.Lazy.Builder as B
import           Hedgehog
import qualified Hedgehog.Gen as Gen
import qualified Hedgehog.Range as Range
import           Text.HTML.Parser
import qualified Text.HTML.TagSoup as Oracle


go :: IO ()
go = do
  _ <- checkSequential $$(discover)
  pure ()


-- * property-test html-parse library

genToken :: Gen Token
genToken = Gen.choice
  [ TagOpen <$> genName <*> genAttrs
  , TagClose <$> genName
  , TagSelfClose <$> genName <*> genAttrs
  , Comment . B.fromText <$> genName
  , Doctype <$> genName
  ]

genTextToken :: Gen Token
genTextToken = Gen.choice
  [ ContentChar <$> Gen.alpha
  , ContentText <$> genName
  ]

genAttrs :: Gen [Attr]
genAttrs = Gen.list (Range.linear 0 12) (Attr <$> genName <*> genName)

genName :: Gen T.Text
genName = Gen.text (Range.exponential 1 12) Gen.alpha


serializer :: [Token] -> String
serializer = TL.unpack . renderTokens

deserializer :: String -> Either String [Token]
deserializer = pure . parseTokens . T.pack


prop_html1 :: Property
prop_html1 = property $ do
  val <- forAll (Gen.list (Range.exponential 0 12) genToken)
  tripping val serializer deserializer

-- add text constructors.  error!
-- use canonicalize to fix the error.
-- do not use tripping, but (===), so rendering happens on non-canonical input.
-- lesson: getting the tests right is sometimes harder than getting the implementation right!

prop_html2 :: Property
prop_html2 = property $ do
  val <- forAll (Gen.list (Range.exponential 0 12) (genToken <|> genTextToken))
  (fmap canonicalize . deserializer . serializer) val === (pure . canonicalize) val

canonicalize :: [Token] -> [Token]
canonicalize = concatTexts . map noChar
  where
    noChar (ContentChar c) = ContentText (T.pack [c])
    noChar tok = tok

    concatTexts (ContentText a : ContentText b : toks)
      = concatTexts $ ContentText (a <> b) : toks
    concatTexts (tok : toks) = tok : canonicalize toks
    concatTexts [] = []


-- * oracles revisited

-- | html-parse has an oracle on hackge!  it is called tagsoup (and you probably
-- should have used that instead of html-parse :-).  tagsoup allows us to do an
-- extended round-trip through two serializer/deserializer pairs:
--
-- >>> html-parse -> tagsoup -> html-parse
prop_html3 :: Property
prop_html3 = property $ do
  val <- forAll (Gen.list (Range.exponential 0 12) (genToken <|> genTextToken))

  (fmap canonicalize' .
   deserializer .
   Oracle.renderTags . Oracle.parseTags .
   serializer) val
    === (pure . canonicalize') val

-- tagsoup will destroy the information whether a tag was self-closing or not.
-- so on top of 'canonicalize', we need something more in 'prop_html3'.

canonicalize' :: [Token] -> [Token]
canonicalize' = f . canonicalize
  where
    f (TagOpen el attr : TagClose el' : toks)
      | el == el'
      = TagSelfClose el attr : f toks
    f (tok : toks) = tok : f toks
    f [] = []


-- other things we could do here.
--
-- - generate arbitrary tagsoup token streams to test for missing legal html
--   documents in the html-parse 'Token' type.
--
-- - if the package used as an oracle provides arbitrary instances for their
--   data type, use those via hedgehog-quickcheck.
--
-- - test html tree in addition to token stream.
--
-- - fuzz serialization to test that we always throw parse errors when we
--   should.
