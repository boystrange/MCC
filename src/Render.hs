{-
┌───────────────────────────────────────────────────────────────────╖
│ This file is part of MC².                                         ║
│                                                                   ║
│ MC² is free software: you can redistribute it and/or modify it    ║
│ under the terms of the GNU General Public License as published by ║
│ the Free Software Foundation, either version 3 of the License, or ║
│ (at your option) any later version.                               ║
│                                                                   ║
│ MC² is distributed in the hope that it will be useful, but        ║
│ WITHOUT ANY WARRANTY; without even the implied warranty of        ║
│ MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU ║
│ General Public License for more details.                          ║
│                                                                   ║
│ You should have received a copy of the GNU General Public License ║
│ along with MC². If not, see <http://www.gnu.org/licenses/>.       ║
│                                                                   ║
│ Copyright 2018 Luca Padovani                                      ║
╘═══════════════════════════════════════════════════════════════════╝
-}

module Render
  ( showPatternVariable
  , printTitle
  , printWarning
  , printOK
  , printInterface
  , printProcessDefinition )
where

import Prelude hiding ((<>))
import Language
import Data.Text.Prettyprint.Doc
import qualified Data.Text.Prettyprint.Doc.Render.String as PR
import qualified Data.Text.Prettyprint.Doc.Render.Terminal as PT
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Char (chr, ord)

----------------------------------
-- PRETTY PRINTER COMPATIBILITY --
----------------------------------

type Document = Doc PT.AnsiStyle

keyword :: String -> Document
keyword = annotate PT.bold . pretty

identifier :: String -> Document
identifier = pretty

constant :: String -> Document
constant = pretty

operator :: String -> Document
operator = annotate PT.bold . pretty

emark :: Document
emark = operator "!"

qmark :: Document
qmark = operator "?"

dot :: Document
dot = operator "."

bar :: Document
bar = operator "|"

triangle :: Document
triangle = operator "▸"

ampersand :: Document
ampersand = operator "&"

---------------
-- UTILITIES --
---------------

subscript :: Int -> String
subscript = map convert . show
  where
    convert :: Char -> Char
    convert ch | ch >= '0' && ch <= '9' = chr (ord ch - ord '0' + ord '₀')
               | ch == '-' = '₋'

embrace :: Document -> Document -> Document -> [Document] -> Document
embrace open close sep ds = align (encloseSep (open <> space) (space <> close) (sep <> space) ds)

sepembrace :: Document -> Document -> Document -> [Document] -> Document
sepembrace open close sep ds = embrace open close sep (map (<> space) (init ds) ++ [last ds])

----------
-- TAGS --
----------

prettyTag :: Tag -> Document
prettyTag = identifier . show

--------------
-- PATTERNS --
--------------

showPatternVariable :: Int -> String
showPatternVariable n =
  [chr $ ord 'α' + n `mod` greekLetters] ++ if n < greekLetters then ""
                                            else subscript (n `div` greekLetters)
  where
    -- how many greek letters we use before turning to subscripts
    greekLetters :: Int
    greekLetters = 12

prettyPattern :: Pattern -> Document
prettyPattern = auxP
  where
    aux :: Char -> Pattern -> Document
    aux _ (PVar n) = identifier (showPatternVariable n)
    aux _ Zero = constant "0"
    aux _ One = constant "1"
    aux _ (Atom tag) = prettyTag tag
    aux op (t :·: s) =
      maybeParens (elem op "*") (cat [aux '·' t <> operator "·", nest 2 (aux '·' s)])
    aux op (t :+: s) =
      maybeParens (elem op "·*") (sep [aux '+' t <+> operator "+", aux '+' s])
    aux _ (Star t) = operator "*" <> aux '*' t

    auxP = aux '_'

    maybeParens :: Bool -> Document -> Document
    maybeParens False = id
    maybeParens True = parens

instance Show Pattern where
  show = PR.renderString . layoutPretty defaultLayoutOptions . prettyPattern

-----------
-- TYPES --
-----------

prettyType :: Type -> Document
prettyType = annotate (PT.colorDull PT.Cyan) . aux
  where
    aux :: Type -> Document
    aux None = identifier "•"
    aux (Part iname cap) = identifier (show iname) <> operator (show cap)
    aux (Type iname cap π) = identifier (show iname) <> operator (show cap) <> prettyPattern π

instance Show Type where
  show = PR.renderString . layoutPretty defaultLayoutOptions . prettyType

----------------
-- INTERFACES --
----------------

printInterface :: IName -> Interface -> IO ()
printInterface iname iface = PT.putDoc renderedInterface >> putStrLn ""
  where
    renderedInterface :: Document
    renderedInterface = group (keyword "interface" <+>
                               identifier (show iname) <+>
                               embrace lbrace rbrace comma messages)

    messages :: [Document]
    messages = map (uncurry prettyMessage) (M.toList iface)

    prettyMessage :: Tag -> [Type] -> Document
    prettyMessage tag ts = prettyTag tag <> parens (hcat $ punctuate comma (map prettyType ts))

---------------
-- PROCESSES --
---------------

printProcessDefinition :: Bool -> PName -> ([O_Name], O_Process) -> IO ()
printProcessDefinition full pname (xs, p) = PT.putDoc prettyDefinition >> putStrLn ""
  where
    prettyDefinition :: Document
    prettyDefinition = nest 2 (vcat [keyword "process" <+> identifier (show pname) <> prettyNames prettyBinding xs <+> equals, prettyProcess p])

    prettyProcess :: O_Process -> Document
    prettyProcess Done = keyword "done"
    prettyProcess (Output u tag vs) = prettyName u <> emark <> prettyTag tag <> prettyNames prettyName vs
    prettyProcess (Input u _ g) = prettyGuard u g
    prettyProcess p@(Parallel _ _) = align (sepembrace lbrace rbrace bar (map prettyProcess (collect p)))
    prettyProcess (New u _ p) = align (sep [keyword "new" <+> prettyBinding u <+> keyword "in", prettyProcess p])
    prettyProcess (Call pname vs) = identifier (show pname) <> prettyNames prettyName vs

    prettyNames :: (O_Name -> Document) -> [O_Name] -> Document
    prettyNames _ [] = emptyDoc
    prettyNames f vs = tupled (map f vs)

    prettyName :: O_Name -> Document
    prettyName = if full then prettyBinding else prettyJustName

    prettyJustName :: O_Name -> Document
    prettyJustName (u, _) = identifier (show u)

    prettyBinding :: O_Name -> Document
    prettyBinding (u, t) = identifier (show u) <+> colon <+> prettyType t

    prettyGuard :: O_Name -> O_Guard -> Document
    prettyGuard u g = align (vsep [renderedCase, sepembrace lbrace rbrace ampersand (map (align . prettySimpleGuard) gs)])
      where
        gs = collectGuards g

        renderedCase :: Document
        renderedCase = keyword "case" <+> prettyName u <+> keyword "of"

    prettySimpleGuard :: O_Guard -> Document
    prettySimpleGuard (Fail msg) = keyword "fail" <+> dquotes (constant msg)
    prettySimpleGuard (Free p) = sep [keyword "free" <+> triangle, prettyProcess p]
    prettySimpleGuard (Select tag xs p) = sep [prettyTag tag <> prettyNames prettyBinding xs <+> triangle, prettyProcess p]

    collectGuards :: O_Guard -> [O_Guard]
    collectGuards (Choice g₁ g₂) = collectGuards g₁ ++ collectGuards g₂
    collectGuards g = [g]

    collect :: O_Process -> [O_Process]
    collect (Parallel p q) = collect p ++ collect q
    collect p = [p]

-----------------------------------
-- AUXILIARY PRINTING OPERATIONS --
-----------------------------------

printNewLine :: IO ()
printNewLine = putStrLn ""

printAnnotatedString :: [PT.AnsiStyle] -> String -> IO ()
printAnnotatedString anns msg = PT.putDoc (foldr annotate (pretty msg) anns)

printTitle :: String -> IO ()
printTitle msg = printAnnotatedString [PT.bold, PT.underlined] msg >> printNewLine

printWarning :: String -> IO ()
printWarning msg = printAnnotatedString [PT.color PT.Red] msg >> printNewLine

printOK :: String -> IO ()
printOK msg = do
  printAnnotatedString [PT.bold, PT.color PT.Green] "PASSED"
  putStrLn $ " (" ++ msg ++ ")"

-----------------
-- CONSTRAINTS --
-----------------

instance Show Constraint where
  show (π₁ :⊑: π₂) = show π₁ ++ " ⊑ " ++ show π₂
