module Text.Mustache.Type

import Lightyear.Position

%access public

data Key = MkKey (List String)

keyToString : Key -> String
keyToString (MkKey []) = "."
-- TODO: optimize
keyToString (MkKey ks) = pack $ intercalate ['.'] $ map unpack ks

Show Key where
  show = keyToString

Eq Key where
  (MkKey k1) == (MkKey k2) = k1 == k2

data PName = MkPName String

Eq PName where
  (MkPName n1) == (MkPName n2) = n1 == n2

data Node =
  ||| Plain text contained between tags
  TextBlock String |
  ||| HTML-escaped variable
  EscapedVar Key |
  ||| Unescaped variable
  UnescapedVar Key |
  ||| Mustache section
  Section Key (List Node) |
  ||| Inverted section
  InvertedSection Key (List Node) |
  ||| Partial with indentation level ('Nothing' means it was inlined)
  Partial PName (Maybe Position)

Show Node where
  show (TextBlock txt) = txt
  show (EscapedVar k) = "{{" ++ show k ++ "}}"
  show (Section k s) = "{{#" ++ show k ++ "}}" ++ show s ++ "{{/" ++ show k ++  "}}"
  show (InvertedSection k s) = "{{^" ++ show k ++ "}}" ++ show s ++ "{{/" ++ show k ++  "}}"
  show _ = "TODO"

Eq Node where
  (TextBlock t1) == (TextBlock t2) = t1 == t2
  (EscapedVar k1) == (EscapedVar k2) = k1 == k2
  (UnescapedVar k1) == (UnescapedVar k2) = k1 == k2
  -- (Section k1 s1) == (Section k2 s2) = k1 == k2 && s1 == s2
  -- (InvertedSection k1 s1) == (InvertedSection k2 s2) = k1 == k2 && s1 == s2
  (Partial n1 p1) == (Partial n2 p2) = n1 == n2 && p1 == p2
  _ == _ = False
