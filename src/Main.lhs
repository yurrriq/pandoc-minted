---
papersize: a5
geometry: margin=2cm
header-includes:
- \setmonofont{Iosevka}
- \usepackage{hyperref}
- \usepackage{minted}
- \newmintinline[hs]{haskell}{}
- \newmintinline[tex]{latex}{}
- \usemintedstyle{lovelace}
---


= pandoc-minted

\textit{A pandoc filter to render \LaTeX\ code blocks using minted}


== Usage

\begin{minted}{fish}
pandoc [OPTIONS] --filter pandoc-minted [FILES]
\end{minted}


== Source

As usual, declare a module \hs{Main}, and \hs{import} some useful definitions,
\hs{intercalate} from \hs{Data.List}, \hs{topDown} from
\hs{Text.Pandoc.Generic}, and everything from \hs{Text.Pandoc.JSON}.

> module Main where


> import           Data.List           (intercalate)
> import           Text.Pandoc.Generic (topDown)
> import           Text.Pandoc.JSON    (Attr, Block(..), Format(..), Inline(..),
>                                       Pandoc, toJSONFilter)


=== The \hs{Minted} Data Type

Define a data type \hs{Minted} to more expressively handle
\hyperref[fn:mintinline]{inline code} and
\hyperref[fn:mintedBlock]{code blocks}.

\label{dt:Minted}

> data Minted
>   = MintInline (String, String) String
>   | MintedBlock (String, String) String


Define a \hs{Show} instance for \hyperref[dt:Minted]{\hs{Minted}}
in order to generate \LaTeX\ code.

> instance Show Minted where
>   show (MintInline (attrs, language) contents)  =
>       "\\mintinline[" ++ attrs ++ "]{" ++ language ++ "}{" ++ contents ++ "}"
>   show (MintedBlock (attrs, language) contents) =
>       unlines [ "\\begin" ++ "{minted}[" ++ attrs ++ "]{" ++ language ++ "}"
>               , contents
>               , "\\end" ++ "{minted}"
>               ]


=== The \hs{main} Function

Run \hyperref[fn:minted]{\hs{minted}} as a JSON filter.

\label{fn:main}

> main :: IO ()
> main = toJSONFilter minted

\label{fn:minted}

> minted :: Maybe Format -> (Pandoc -> Pandoc)
> minted (Just (Format "latex")) = topDown (concatMap mintinline) .
>                                  topDown (concatMap mintedBlock)
> minted _                       = id


==== Handle Inline Code

Transform \hs{Code} into a \tex{\mintinline} call, otherwise return
a given \hs{Inline}.

\label{fn:mintinline}

> mintinline :: Inline -> [Inline]
> mintinline (Code attr contents) =
>     let
>       latex = MintInline (unpackCode attr "text") contents
>     in
>     [ RawInline (Format "latex") (show latex) ]
> mintinline x                    = [x]


==== Handle Code Blocks

Transform a \hs{CodeBlock} into a \tex{minted} environment,
otherwise return a given \hs{Block}.

\label{fn:mintedBlock}

> mintedBlock :: Block -> [Block]
> mintedBlock (CodeBlock attr contents) =
>     let
>         latex = MintedBlock (unpackCode attr "text") contents
>     in
>         [ RawBlock (Format "latex") (show latex) ]
> mintedBlock x                         = [x]


\newpage
=== Helper Functions

Given a triplet of \hs{Attr}ibutes (identifier, language(s), and key/value pairs)
and a default language, return a pair of \tex{minted} attributes and language.

> unpackCode :: Attr -> String -> (String, String)
> unpackCode (_, [], kvs) defaultLanguage = (unpackAttrs kvs, defaultLanguage)
> unpackCode (identifier, "sourceCode" : "literate" : language : _, kvs) _ =
>     (unpackAttrs kvs, language)
> unpackCode (identifier, "sourceCode" : language : _, kvs) _ =
>     (unpackAttrs kvs, language)
> unpackCode (_, language : _, kvs) _     = (unpackAttrs kvs, language)

Given a list of key/value pairs, return a string suitable for \tex{minted}
options.

> unpackAttrs :: [(String, String)] -> String
> unpackAttrs kvs = intercalate ", " [ k ++ "=" ++ v  | (k, v) <- kvs ]
