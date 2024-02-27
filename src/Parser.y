{
{-# OPTIONS -w #-}
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

module Parser (parseProcess) where

import Lexer
import Language

import Data.Either (partitionEithers)
import Control.Exception
}

%name parse
%tokentype { Token }
%monad { Alex }
%lexer { lexwrap } { Token _ TokenEOF }
%error { happyError }

%token
  DONE      { Token _ TokenDone }
  IN        { Token _ TokenIn }
  INTERFACE { Token _ TokenInterface }
  FAIL      { Token _ TokenFail }
  FREE      { Token _ TokenFree }
  CASE      { Token _ TokenCase }
  OF        { Token _ TokenOf }
  NEW       { Token _ TokenNew }
  PROCESS   { Token _ TokenProcess }
  INT       { Token _ (TokenInt $$) }
  STRING    { Token _ (TokenString $$) }
  CID       { $$@(Token _ (TokenCID _)) }
  LID       { $$@(Token _ (TokenLID _)) }
  '='       { Token _ TokenEQ }
  '▸'       { Token _ TokenTriangle }
  '.'       { Token _ TokenDot }
  ':'       { Token _ TokenColon }
  ';'       { Token _ TokenSemiColon }
  ','       { Token _ TokenComma }
  '|'       { Token _ TokenBar }
  '('       { Token _ TokenLParen }
  ')'       { Token _ TokenRParen }
  '{'       { Token _ TokenLBrace }
  '}'       { Token _ TokenRBrace }
  '['       { Token _ TokenLBrack }
  ']'       { Token _ TokenRBrack }
  '+'       { Token _ TokenPlus }
  '·'       { Token _ TokenCDot }
  '*'       { Token _ TokenStar }
  '&'       { Token _ TokenAmp }
  '?'       { Token _ TokenQMark }
  '!'       { Token _ TokenEMark }

%nonassoc '}' ']' IN ELSE

%right '&'
%right '|'
%right ','
%right '+'
%right '·'
%nonassoc '*'
%left '.'

%%

--------------
-- PROGRAMS --
--------------

Program
  : InterfaceList ProcessDefList { ($1, $2) }

---------------
-- PROCESSES --
---------------

Process
  : DONE { Done }
  | '{' Process '}' { $2 }
  | Name '!' Tag Names { Output $1 $3 $4 }
  | Name '?' SimpleGuard { Input $1 (fst $3) (snd $3) }
  | CASE Name '?' Pattern OF '{' Guard '}' { Input $2 $4 $7 }
  | Process '|' Process { Parallel $1 $3 }
  | NEW Name ':' InterfaceName IN Process { New $2 $4 $6 }
  | ProcessName Names { Call $1 $2 }

Names
  : { [] }
  | '(' ')' { [] }
  | '(' NameNeList ')' { $2 }

NameNeList
  : Name { [$1] }
  | Name ',' NameNeList { $1 : $3 }

Guard
  : FAIL STRING { Fail $2 }
  | FREE '▸' Process { Free $3 }
  | Tag Names '▸' Process { Select $1 $2 $4 }
  | Guard '&' Guard { Choice $1 $3 }

SimpleGuard
  : FAIL STRING { (Zero, Fail $2) }
  | FREE '.' Process { (One, Free $3) }

---------------------------
-- INTERFACE DEFINITIONS --
---------------------------

InterfaceList
  : { [] }
  | Interface InterfaceList { $1 : $2 }

Interface
  : INTERFACE InterfaceName '{' MessageList '}' { ($2, $4) }

MessageList
  : { [] }
  | MessageNeList { $1 }

MessageNeList
  : Message { [$1] }
  | Message ',' MessageNeList { $1 : $3 }

Message
  : Tag Types { Message $1 $2 }

Types
  : { [] }
  | '(' TypeList ')' { $2 }

TypeList
  : { [] }
  | TypeNeList { $1 }

TypeNeList
  : Type { [$1] }
  | Type ',' TypeNeList { $1 : $3 }

-------------------------
-- PROCESS DEFINITIONS --
-------------------------

ProcessDefList
  : { [] }
  | ProcessDef ProcessDefList { $1 : $2 }

ProcessDef
  : PROCESS ProcessName Parameters '=' Process { ($2, $3, $5) }

Parameters
  : { [] }
  | '(' ParameterList ')' { $2 }

ParameterList
  : { [] }
  | ParameterNeList { $1 }

ParameterNeList
  : Parameter { [$1] }
  | ParameterNeList ',' ParameterNeList { $1 ++ $3 }

Parameter
  : Name ':' Type { ($1, $3) }

-----------------
-- IDENTIFIERS --
-----------------

Name
  : LID { Identifier (At $ getPos $1) (getId $1) :: Name }

InterfaceName
  : CID { Identifier (At $ getPos $1) (getId $1) :: IName }

ProcessName
  : CID { Identifier (At $ getPos $1) (getId $1) :: PName }

Tag
  : CID { Identifier (At $ getPos $1) (getId $1) :: Tag }

-----------
-- TYPES --
-----------

Type
  : InterfaceName Capability { Part $1 $2 }
  | InterfaceName Capability Pattern { Type $1 $2 $3 }

Capability
  : '?' { In }
  | '!' { Out }

--------------
-- PATTERNS --
--------------

Pattern
  : INT
    {% if $1 == 0 then return Zero
       else if $1 == 1 then return One
       else fail $ show $1 ++ " is an invalid pattern"  }
  | Tag { Atom $1 }
  | Pattern '+' Pattern { $1 :+: $3 }
  | Pattern '·' Pattern { $1 :·: $3 }
  | '*' Pattern { Star $2 }
  | '(' Pattern ')' { $2 }

{
getId :: Token -> String
getId (Token _ (TokenLID x)) = x
getId (Token _ (TokenCID x)) = x

getPos :: Token -> (Int, Int)
getPos (Token (AlexPn _ line col) _) = (line, col)

lexwrap :: (Token -> Alex a) -> Alex a
lexwrap = (alexMonadScan' >>=)

happyError :: Token -> Alex a
happyError (Token p t) = alexError' p ("parse error at token '" ++ show t ++ "'")

parseProcess :: FilePath -> String -> Either String ([S_Interface], [S_ProcessDefinition])
parseProcess = runAlex' parse
}
