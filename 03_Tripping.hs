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

genAttrs :: Gen [Attr]
genAttrs = Gen.list (Range.linear 0 12) (Attr <$> genName <*> genName)

genName :: Gen T.Text
genName = Gen.text (Range.exponential 1 12) Gen.alpha


serializer :: [Token] -> String
serializer = TL.unpack . renderTokens

deserializer :: String -> Either String [Token]
deserializer = pure . parseTokens . T.pack


prop_html1 :: Property
prop_html1 = undefined

prop_html2 :: Property
prop_html2 = undefined


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
prop_html3 = undefined


canonicalize' :: [Token] -> [Token]
canonicalize' = f . canonicalize
  where
    f (TagOpen el attr : TagClose el' : toks)
      | el == el'
      = TagSelfClose el attr : f toks
    f (tok : toks) = tok : f toks
    f [] = []
