module Test.ParserSpec

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Position
import Lightyear.Strings
import Specdris.Spec
import Text.Mustache.Parser
import Text.Mustache.Type

%access export

shouldParse : (Eq a, Show a)
            => Either String a
            -> a
            -> SpecResult
shouldParse actual expected =
  case actual of
    Right result => result === expected
    Left e => UnaryFailure actual $ "expected: " ++ show expected ++ "\nbut parsing failed with error:\n" ++ e

shouldNotParse : (Eq a, Show a)
               => Either String a
               -> SpecResult
shouldNotParse actual =
  case actual of
    Left e => Success
    Right result => UnaryFailure result $ "expected parser to fail, but it cucceeded with: "

p : String -> Either String (List Node)
p = parseMustache Nothing

key : String -> Key
key = MkKey . pure

testParse : IO ()
testParse = spec $ do
  -- let key = Key . pure
  it "parses text" $
    p "test12356p0--=-34{}jnv,\n"
      `shouldParse` [TextBlock "test12356p0--=-34{}jnv,\n"]
  describe "when parsing a variable" $ do
    describe "with white space" $ do
      it "parses escaped {{ variable }}" $
        p "{{ name }}" `shouldParse` [EscapedVar (key "name")]
      it "parses unescaped {{{ variable }}}" $
        p "{{{ name }}}" `shouldParse` [UnescapedVar (key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{& name }}" `shouldParse` [UnescapedVar (key "name")]
    describe "without white space" $ do
      it "parses escaped {{variable}}" $
        p "{{name}}" `shouldParse` [EscapedVar (key "name")]
      it "parses unescaped {{{variable}}}" $
        p "{{{name}}}" `shouldParse` [UnescapedVar (key "name")]
      it "parses unescaped {{& variable }}" $
        p "{{&name}}" `shouldParse` [UnescapedVar (key "name")]
    it "allows '-' in variable names" $
      p "{{ var-name }}" `shouldParse` [EscapedVar (key "var-name")]
    it "allows '_' in variable names" $
      p "{{ var_name }}" `shouldParse` [EscapedVar (key "var_name")]
  describe "when parsing a section" $ do
    it "parses empty section" $
      p "{{#section}}{{/section}}" `shouldParse` [Section (key "section") []]
    it "parses non-empty section" $
      p "{{# section }}Hi, {{name}}!\n{{/section}}" `shouldParse`
        [Section (key "section")
         [ TextBlock "Hi, "
         , EscapedVar (key "name")
         , TextBlock "!\n"]]
  describe "when parsing an inverted section" $ do
    it "parses empty inverted section" $
      p "{{^section}}{{/section}}" `shouldParse`
        [InvertedSection (key "section") []]
    it "parses non-empty inverted section" $
      p "{{^ section }}No one here?!\n{{/section}}" `shouldParse`
        [InvertedSection (key "section") [TextBlock "No one here?!\n"]]
  describe "when parsing a partial" $ do
    it "parses a partial with white space" $
      p "{{> that-s_my-partial }}" `shouldParse`
        [Partial (MkPName "that-s_my-partial") (Just $ MkPos Nothing 1 1)]
    it "parses a partial without white space" $
      p "{{>that-s_my-partial}}" `shouldParse`
        [Partial (MkPName "that-s_my-partial") (Just $ MkPos Nothing 1 1)]
    it "handles indented partial correctly" $
      p "   {{> next_one }}" `shouldParse`
        [Partial (MkPName "next_one") (Just $ MkPos Nothing 1 4)]
  describe "when running into delimiter change" $ do
    it "has effect" $
      p "{{=<< >>=}}<<var>>{{var}}" `shouldParse`
        [EscapedVar (key "var"), TextBlock "{{var}}"]
    it "handles whitespace just as well" $
      p "{{=<<   >>=}}<<  var >>{{ var  }}" `shouldParse`
        [EscapedVar (key "var"), TextBlock "{{ var  }}"]
    it "affects {{{s" $
      p "{{=<< >>=}}<<{var}>>" `shouldParse`
        [UnescapedVar (key "var")]
    it "parses two subsequent delimiter changes" $
      p "{{=(( ))=}}(( var ))((=-- $-=))--#section$---/section$-" `shouldParse`
        [EscapedVar (key "var"), Section (key "section") []]
    it "propagates delimiter change from a nested scope" $
      p "{{#section}}{{=<< >>=}}<</section>><<var>>" `shouldParse`
        [Section (key "section") [], EscapedVar (key "var")]
  describe "when given malformed input" $ do
    it "rejects unclosed tags" $ do
      let s = "{{ name "
      shouldNotParse $ p s
    it "rejects unknown tags" $ do
      let s = "{{? boo }}"
      shouldNotParse $ p s