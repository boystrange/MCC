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

module Exceptions where

import Language
import Render
import Control.Exception
import Data.List (intersperse)

data MyException
  = ErrorSyntax String
  | ErrorMultipleInterfaceDefinitions IName
  | ErrorMultipleMessageDefinitions Tag
  | ErrorMultipleProcessDefinitions PName
  | ErrorMultipleNameDeclarations Name
  | ErrorUnknownIdentifier String String
  | ErrorTypeMismatch String
  | ErrorArityMismatch Pos String Int Int
  | ErrorInvalidType String
  | ErrorCyclicDependencyGraph String

instance Exception MyException

instance Show MyException where
  show (ErrorSyntax msg) = msg
  show (ErrorMultipleMessageDefinitions tag) = "multiple message definitions: " ++ showWithPos tag
  show (ErrorMultipleInterfaceDefinitions iname) = "multiple interface definitions: " ++ showWithPos iname
  show (ErrorMultipleProcessDefinitions pname) = "multiple process definitions: " ++ showWithPos pname
  show (ErrorUnknownIdentifier kind name) = "unknown " ++ kind ++ ": " ++ name
  show (ErrorMultipleNameDeclarations u) = "multiple declarations: " ++ showWithPos u
  show (ErrorTypeMismatch msg) = "type error: " ++ msg
  show (ErrorArityMismatch pos kind expected actual) =
    show pos ++ "arity mismatch for " ++ kind ++ ": expected " ++
    show expected ++ ", actual " ++ show actual
  show (ErrorCyclicDependencyGraph msg) = "cyclic dependencies: " ++ msg
  show (ErrorInvalidType msg) = "invalid type: " ++ msg
