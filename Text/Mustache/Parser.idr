module Text.Mustache.Parser

import Lightyear
import Lightyear.Char
import Lightyear.Combinators
import Lightyear.Position
import Lightyear.Strings
import public Text.Mustache.Type

-- TODO: configurable in original
openingDel : String
openingDel = "{{"

-- TODO: configurable in original
closingDel : String
closingDel = "}}"

pBol : Parser ()
pBol = do
  level <- colNo <$> getPosition
  when (level /= 1) empty

someTill : Monad m => ParserT String m a -> ParserT String m b -> ParserT String m (List a)
someTill p end = (::) <$> p <*> manyTill p end

-- TODO: PR to Lightyear?
pLookAhead : ParserT str m tok -> ParserT str m tok
pLookAhead (PT f) =
  PT $ \r, us, cs, ue, ce, st =>
          f r
          (\t, s => us t st)
          (\t, s => cs t st)
          ue
          ce
          st

pTextBlock : Parser Node
pTextBlock = do
  requireFailure (string openingDel)
  -- FIXME: laziness?
  let terminator = choice $ the (List (Parser ()))
        [ pure () <* pLookAhead (string openingDel)
        --, pBol -- TODO: what's that for?
        , eof
        ]
  TextBlock . pack <$> someTill anyChar terminator

sc :  (Monad m, Stream Char s) => ParserT s m ()
sc = skip (many (satisfy spc)) <?> "space or tab"
  where spc c = c == ' ' || c == '\t'

pKey : Parser Key
pKey = (map MkKey . lexeme) (implicit_ <|>| other <?> "key")
  where
    implicit_ = pure [] <* char '.'
    other : Parser (List String)
    other = sepBy1 (pack <$> many (alphaNum <|> char '-' <|> char '_' <?> "alphanumeric char or '-' or '_'")) (char '.')

pTag : String -> Parser Key
pTag suffix = do
  between (token $ openingDel <+> suffix) (string closingDel) pKey

pUnescapedVariable : Parser Node
pUnescapedVariable = UnescapedVar <$> pTag "&"

pUnescapedSpecial : Parser Node
pUnescapedSpecial = do
  between (token $ openingDel <+> "{") (string $ "}" <+> closingDel) $
    UnescapedVar <$> pKey

pClosingTag : Key -> Parser ()
pClosingTag key = do
  let str = keyToString key
  between (token $ openingDel <+> "/") (string closingDel) (string str)
  pure ()

-- TODO: whitespace after >
pPartial : (Position -> Maybe Position) -> Parser Node
pPartial f = do
  pos <- f <$> getPosition
  key <- pTag ">" <?> "partial tag"
  let pname = MkPName $ keyToString key
  pure (Partial pname pos)

pComment : Parser ()
pComment = do
  token (openingDel <+> "!")
  manyTill anyChar (string closingDel)
  pure ()

pEscapedVariable : Parser Node
pEscapedVariable = EscapedVar <$> pTag ""

eol : Parser Char
eol = newline <|> crlf

pStandalone : Parser a -> Parser a
pStandalone p = -- pBol *>
  between sc (sc <* ((pure () <* eol) <|> eof)) p

withStandalone : Parser a -> Parser a
withStandalone p = pStandalone p <|> p

mutual
  pSection : String -> (Key -> List Node -> Node) -> Parser Node
  pSection suffix f = do
    key   <- withStandalone (pTag suffix)
    nodes <- (pMustache . withStandalone . pClosingTag) key
    pure (f key nodes)

  public
  pMustache : Parser () -> Parser (List Node)
  pMustache = map catMaybes . manyTill (choice alts)
    where
      alts : List (Parser (Maybe Node))
      alts =
        [ pure Nothing <* withStandalone pComment
        , Just    <$> pSection "#" Section
        , Just    <$> pSection "^" InvertedSection
        , Just    <$> pStandalone (pPartial Just)
        , Just    <$> pPartial (const Nothing)
        -- , pure Nothing <*  withStandalone pSetDelimiters
        , Just    <$> pUnescapedVariable
        , Just    <$> pUnescapedSpecial
        , Just    <$> pEscapedVariable
        , Just    <$> pTextBlock
        ]

pTest : Parser (List String)
pTest = many $ string "frob"

test1 : String
test1 = """
Hello {{name}}
You have just won {{value}} dollars!
{{#in_ca}}
Well!, {{taxed_value}} dollars, after taxes.
{{/in_ca}}
"""