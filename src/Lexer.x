{
{-# OPTIONS -w  #-}
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

module Lexer
  ( Token(..)
  , AlexPosn(..)
  , TokenClass(..)
  , Alex(..)
  , runAlex'
  , alexMonadScan'
  , alexError'
  ) where

import Prelude hiding (lex)
import Control.Monad (liftM)
}

%wrapper "monadUserState"

$digit   = 0-9
$extra   = [₀-₉⁰-⁹⁺⁻₊₋]
$alpha   = [A-Za-z]
@lower   = [a-z]
@upper   = [A-Z]
@next    = $alpha | $digit | $extra | \_ | \'
@lid     = @lower @next*
@cid     = @upper @next*
@int     = $digit+
@float   = (@int \. @int) | (@int \. @int)
@string  = \"[^\"]*\"

tokens :-
  $white+ ;
  "//".*  ;
  "."     { lex' TokenDot         }
  ","     { lex' TokenComma       }
  ":"     { lex' TokenColon       }
  ";"     { lex' TokenSemiColon   }
  "("     { lex' TokenLParen      }
  ")"     { lex' TokenRParen      }
  "{"     { lex' TokenLBrace      }
  "}"     { lex' TokenRBrace      }
  "["     { lex' TokenLBrack      }
  "]"     { lex' TokenRBrack      }
  "="     { lex' TokenEQ          }
  "+"     { lex' TokenPlus        }
  "·"     { lex' TokenCDot        }
  "*"     { lex' TokenStar        }
  "?"     { lex' TokenQMark       }
  "!"     { lex' TokenEMark       }
  "|"     { lex' TokenBar         }
  "&"     { lex' TokenAmp         }
  "▸"     { lex' TokenTriangle    }
  @string { lex (TokenString . tail . init) }
  @lid    { lex lookupLID         }
  @cid    { lex TokenCID          }
  @int    { lex (TokenInt . read) }
  @int "." { lex (TokenDouble . fromIntegral . read . init) }
  @float  { lex (TokenDouble . read) }

{
-- To improve error messages, We keep the path of the file we are
-- lexing in our own state.
data AlexUserState = AlexUserState { filePath :: FilePath }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState "<unknown>"

getFilePath :: Alex FilePath
getFilePath = liftM filePath alexGetUserState

setFilePath :: FilePath -> Alex ()
setFilePath = alexSetUserState . AlexUserState

keywords :: [(String, TokenClass)]
keywords = [("done",      TokenDone),
            ("process",   TokenProcess),
            ("in",        TokenIn),
            ("new",       TokenNew),
            ("free",      TokenFree),
            ("fail",      TokenFail),
            ("case",      TokenCase),
            ("of",        TokenOf),
            ("interface", TokenInterface)]

lookupLID :: String -> TokenClass
lookupLID s = case lookup s keywords of
                Nothing -> TokenLID s
                Just tok -> tok

-- The token type, consisting of the source code position and a token class.
data Token = Token AlexPosn TokenClass
  deriving (Show)

data TokenClass
  = TokenDone
  | TokenProcess
  | TokenNew
  | TokenIn
  | TokenCase
  | TokenOf
  | TokenInterface
  | TokenMessage
  | TokenFree
  | TokenFail
  | TokenInt Int
  | TokenDouble Double
  | TokenString String
  | TokenLID String
  | TokenCID String
  | TokenEQ
  | TokenPlus
  | TokenMinus
  | TokenTimes
  | TokenCDot
  | TokenTriangle
  | TokenStar
  | TokenDot
  | TokenBar
  | TokenAmp
  | TokenComma
  | TokenColon
  | TokenSemiColon
  | TokenLParen
  | TokenRParen
  | TokenLBrace
  | TokenRBrace
  | TokenLBrack
  | TokenRBrack
  | TokenQMark
  | TokenEMark
  | TokenEOF
  deriving (Show)

alexEOF :: Alex Token
alexEOF = do
  (p,_,_,_) <- alexGetInput
  return $ Token p TokenEOF

-- Unfortunately, we have to extract the matching bit of string
-- ourselves...
lex :: (String -> TokenClass) -> AlexAction Token
lex f = \(p,_,_,s) i -> return $ Token p (f (take i s))

-- For constructing tokens that do not depend on
-- the input
lex' :: TokenClass -> AlexAction Token
lex' = lex . const

-- We rewrite alexMonadScan' to delegate to alexError' when lexing fails
-- (the default implementation just returns an error message).
alexMonadScan' :: Alex Token
alexMonadScan' = do
  inp <- alexGetInput
  sc <- alexGetStartCode
  case alexScan inp sc of
    AlexEOF -> alexEOF
    AlexError (p, _, _, s) ->
        alexError' p ("lexical error at character '" ++ take 1 s ++ "'")
    AlexSkip  inp' len -> do
        alexSetInput inp'
        alexMonadScan'
    AlexToken inp' len action -> do
        alexSetInput inp'
        action (ignorePendingBytes inp) len

-- Signal an error, including a commonly accepted source code position.
alexError' :: AlexPosn -> String -> Alex a
alexError' (AlexPn _ l c) msg = do
  fp <- getFilePath
  alexError (fp ++ ":" ++ show l ++ ":" ++ show c ++ ": " ++ msg)

-- A variant of runAlex, keeping track of the path of the file we are lexing.
runAlex' :: Alex a -> FilePath -> String -> Either String a
runAlex' a fp input = runAlex input (setFilePath fp >> a)
}
