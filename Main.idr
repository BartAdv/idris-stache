module Main

import Lightyear.Core
import Lightyear.Combinators
import Lightyear.Strings as LS
import Text.Mustache.Parser

test1 : String
test1 = """
Hello {{name}}
You have just won {{value}} dollars!
{{#in_ca}}
Well!, {{taxed_value}} dollars, after taxes.
{{/in_ca}}
"""

comprehensive : String
comprehensive = """
This is a comprehensive example of Mustache template.

It has {{first-thing}} and {{&second-thing}}, as well as a bit of
{{third-thing}}.

John has the following items today:
{{#items}}
* {{.}}
{{/items}}
{{^items}}
None.
{{/items}}

We could also use this text as a start of our conversation:

{{>lorem-ipsum}}

But we won't.
"""

main : IO ()
main = do
  fname <- getLine
  Right txt <- readFile fname
             | putStrLn "Can't read file"
  putStrLn txt
  case LS.parse (pMustache eof) txt of
    Right res => putStrLn ("Successfully parsed " <+> show (length res) <+> " nodes!")
    Left err => putStrLn err
  pure ()
