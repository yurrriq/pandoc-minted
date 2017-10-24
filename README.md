pandoc-minted
=============

*A pandoc filter to render LaTeX code blocks using minted*

Usage
-----

``` fish
pandoc ... --filter pandoc-minted ...
```

Source
------

As usual, declare a `module Main`...

```haskell
module Main where
```

... and `import` some useful definitions, in this case, `intercalate` from `Data.List`, `topDown` from `Text.Pandoc.Generic` and everything from `Text.Pandoc.JSON`.

```haskell
import           Data.List           (intercalate)
import           Text.Pandoc.Generic (topDown)
import           Text.Pandoc.JSON
```

Define a data type `Minted` to more expressively handle inline code and code blocks.

```haskell
data Minted
  = MintedInline (String, String) String
  | MintedBlock (String, String) String
```

```haskell
instance Show Minted where
  show (MintedInline (attrs, language) contents) =
    "\\mintinline[" ++ attrs ++ "]{" ++ language ++ "}{" ++ contents ++ "}"
  show (MintedBlock (attrs, language) contents) =
    unlines [ "\\begin{minted}[" ++ attrs ++ "]{" ++ language ++ "}"
            , contents
            , "\\end{minted}"
            ]
```

```haskell
main :: IO ()
main = toJSONFilter minted
```

```haskell
minted :: Pandoc -> Pandoc
minted = topDown (concatMap mintinline) .
         topDown (concatMap mintedBlock)
```

```haskell
mintinline :: Inline -> [Inline]
mintinline (Code attr contents) =
  let
    latex = show $ MintedInline (unpackCode attr "text") contents
  in
    [ RawInline (Format "latex") latex ]
mintinline x = [x]
```

```haskell
mintedBlock :: Block -> [Block]
mintedBlock (CodeBlock attr contents) =
  let
    latex = show $ MintedBlock (unpackCode attr "text") contents
  in
    [ RawBlock (Format "latex") latex ]
mintedBlock x = [x]
```

```haskell
unpackCode :: Attr -> String -> (String, String)
unpackCode (_, [], kvs) defaultLanguage =
  (unpackAttrs kvs, defaultLanguage)
unpackCode (identifier, "sourceCode" : _, kvs) defaultLanguage =
  unpackCode (identifier, ["idris"], kvs) defaultLanguage
unpackCode (_, language : _, kvs) _ =
  (unpackAttrs kvs, language)
```

```haskell
unpackAttrs :: [(String, String)] -> String
unpackAttrs kvs = intercalate ", " [ k ++ "=" ++ v  | (k, v) <- kvs ]
```
