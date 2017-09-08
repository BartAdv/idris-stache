module Text.Mustache.Parser

import Control.Monad.State
import Lightyear
import Lightyear.Char
import Lightyear.Combinators
import Lightyear.Position
import Lightyear.Strings
import public Text.Mustache.Type

-- the one from Lightyear.Strings
%hide Parser

record Delimiters where
  constructor MkDelimiters
  openingDel : String
  closingDel : String

Parser : Type -> Type
Parser = ParserT String (State Delimiters)

pBol : Monad m => ParserT str m ()
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
          (\t, s => cs t st) -- cs or us ??
          ue
          ce
          st

pTextBlock : Parser Node
pTextBlock = do
  start <- gets openingDel
  requireFailure (string start)
  -- FIXME: laziness?
  let terminator = choice $ the (List (Parser ()))
        [ pure () <* pLookAhead (string start)
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
  start <- gets openingDel
  end <- gets closingDel
  between (token $ start <+> suffix) (string end) pKey

pUnescapedVariable : Parser Node
pUnescapedVariable = UnescapedVar <$> pTag "&"

pUnescapedSpecial : Parser Node
pUnescapedSpecial = do
  start <- gets openingDel
  end <- gets closingDel
  between (token $ start <+> "{") (string $ "}" <+> end) $
    UnescapedVar <$> pKey

pClosingTag : Key -> Parser ()
pClosingTag key = do
  start <- gets openingDel
  end   <- gets closingDel
  let str = keyToString key
  between (token $ start <+> "/") (string end) (string str)
  pure ()

pPartial : (Position -> Maybe Position) -> Parser Node
pPartial f = do
  pos <- f <$> getPosition
  key <- pTag ">" <?> "partial tag"
  let pname = MkPName $ keyToString key
  pure (Partial pname pos)

pComment : Parser ()
pComment = do
  start <- gets openingDel
  end <- gets closingDel
  token (start <+> "!")
  manyTill anyChar (string end)
  pure ()

pEscapedVariable : Parser Node
pEscapedVariable = EscapedVar <$> pTag ""

eol : (Stream Char str, Monad m) => ParserT str m Char
eol = newline <|> crlf

pStandalone : (Stream Char str, Monad m) => ParserT str m a -> ParserT str m a
pStandalone p = -- pBol *>
  between sc (sc <* ((pure () <* eol) <|> eof)) p

withStandalone : Parser a -> Parser a
withStandalone p = pStandalone p <|> p

pDelimiter : Parser String
pDelimiter = pack <$> some (satisfy delChar) <?> "delimiter"
  where delChar x = not (isSpace x) && x /= '='

pSetDelimiters : Parser ()
pSetDelimiters = do
  start <- gets openingDel
  end   <- gets closingDel
  token (start <+> "=")
  start' <- pDelimiter <* spaces
  end'   <- pDelimiter <* spaces
  token ("=" <+> end)
  put (MkDelimiters start' end')

mutual
  pSection : String -> (Key -> List Node -> Node) -> Parser Node
  pSection suffix f = do
    key   <- withStandalone (pTag suffix)
    nodes <- (pMustache . withStandalone . pClosingTag) key
    pure (f key nodes)

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
        , pure Nothing <* pSetDelimiters
        , Just    <$> pUnescapedVariable
        , Just    <$> pUnescapedSpecial
        , Just    <$> pEscapedVariable
        , Just    <$> pTextBlock
        ]

export
parseMustache : Maybe String -> String -> Either String (List Node)
parseMustache name src =
  let initialDelims = MkDelimiters "{{" "}}"
      Id (r,s) = flip runStateT initialDelims $ execParserT (pMustache eof) (initialState name src 8)
  in case r of
    MkReply _ (Success x)  => Right x
    MkReply _ (Failure es) => Left $ concat $ intersperse "\n" $ map display es

testParse : Parser a -> String -> Either String (a, Delimiters)
testParse p src =
  let initialDelims = MkDelimiters "{{" "}}"
      Id (r,s) = flip runStateT initialDelims $ execParserT p (initialState Nothing src 8)
  in case r of
    MkReply _ (Success x)  => Right (x,s)
    MkReply _ (Failure es) => Left $ concat $ intersperse "\n" $ map display es