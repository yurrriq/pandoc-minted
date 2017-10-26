= pandoc-minted

*A pandoc filter to render LaTeX code blocks using minted*


== Usage

```fish
pandoc ... --filter pandoc-minted ...
```


== Source

As usual, declare a `module Main`...

> module Main where

... and `import` some useful definitions:

- `intercalate` from `Data.List`,

> import           Data.List           (intercalate)

- `topDown` from `Text.Pandoc.Generic`,

> import           Text.Pandoc.Generic (topDown)

- and everything from `Text.Pandoc.JSON`.

> import           Text.Pandoc.JSON


=== The `Minted` Data Type

Define a data type `Minted` to more expressively handle
[inline code](#mintinline) and [code blocks](#mintedBlock).

<a name="Minted" />

> data Minted
>   = MintedInline (String, String) String
>   | MintedBlock (String, String) String

Define a `Show` instance for `Minted`, in order to generate LaTeX code.

> instance Show Minted where
>   show (MintedInline (attrs, language) contents) =
>     "\\mintinline[" ++ attrs ++ "]{" ++ language ++ "}{" ++ contents ++ "}"
>   show (MintedBlock (attrs, language) contents) =
>     unlines [ "\\begin{minted}[" ++ attrs ++ "]{" ++ language ++ "}"
>             , contents
>             , "\\end{minted}"
>             ]


=== The `main` Function

Run [`minted`](#minted) as a JSON filter.

<a name="main" />

> main :: IO ()
> main = toJSONFilter minted

<a name="minted" />

> minted :: Pandoc -> Pandoc
> minted = topDown (concatMap mintinline) .
>          topDown (concatMap mintedBlock)

=== Handle Inline Code

Transform a `Code` into a `\mintinline` call, otherwise return a given `Inline`.

<a name="mintinline" />

> mintinline :: Inline -> [Inline]
> mintinline (Code attr contents) =
>   let
>     latex = show $ MintedInline (unpackCode attr "text") contents
>   in
>     [ RawInline (Format "latex") latex ]
> mintinline x = [x]

=== Handle Code Blocks

Transform a `CodeBlock` into a `minted` environment,
otherwise return a given `Block`.

<a name="mintedBlock" />

> mintedBlock :: Block -> [Block]
> mintedBlock (CodeBlock attr contents) =
>   let
>     latex = show $ MintedBlock (unpackCode attr "text") contents
>   in
>     [ RawBlock (Format "latex") latex ]
> mintedBlock x = [x]


=== Helper Functions

Given a triplet of `Attr`ibutes (identifier, language(s), and key/value pairs)
and a default language, return a pair of `minted` attributes and language.

> unpackCode :: Attr -> String -> (String, String)
> unpackCode (_, [], kvs) defaultLanguage =
>   (unpackAttrs kvs, defaultLanguage)
> unpackCode (identifier, "sourceCode" : _, kvs) defaultLanguage =
>   unpackCode (identifier, ["idris"], kvs) defaultLanguage
> unpackCode (_, language : _, kvs) _ =
>   (unpackAttrs kvs, language)

Given a list of key/value pairs, return a string suitable for `minted` options.

> unpackAttrs :: [(String, String)] -> String
> unpackAttrs kvs = intercalate ", " [ k ++ "=" ++ v  | (k, v) <- kvs ]
