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

module Validation where

import qualified Data.Map as M
import Control.Monad (forM_, mapM_)
import Control.Exception

import Language
import Exceptions
import Pattern

validate :: PatternMap -> InterfaceEnv -> ProcessDefinitions -> IO (InterfaceEnv, ProcessDefinitions)
validate solution ienv ps = do
  checkIs ienv'
  checkPs ps'
  return (ienv', ps')
  where
    typeMap :: Type -> Type
    typeMap None = None
    typeMap (Part iname cap) = Part iname cap
    typeMap (Type iname cap π) = Type iname cap (simplify (substAll solution π))

    ienv' :: InterfaceEnv
    ienv' = M.map (mapInterface typeMap) ienv

    ps' = mapProcessDefinitions typeMap ps

    checkP :: O_Process -> IO ()
    checkP Done = return ()
    checkP (Output _ _ vs) = checkNs vs
    checkP (Input _ _ g) = checkG g
    checkP (Parallel p₁ p₂) = checkP p₁ >> checkP p₂
    checkP (New _ _ p) = checkP p
    checkP (Call _ vs) = checkNs vs

    checkG :: O_Guard -> IO ()
    checkG (Fail _) = return ()
    checkG (Free p) = checkP p
    checkG (Select _ xs p) = checkNs xs >> checkP p
    checkG (Choice g₁ g₂) = checkG g₁ >> checkG g₂

    checkN :: O_Name -> IO ()
    checkN (u, t) = checkT (showWithPos u) t

    checkNs :: [O_Name] -> IO ()
    checkNs = mapM_ checkN

    checkT :: String -> Type -> IO ()
    checkT x (Type _ _ π) | zero π = throw $ ErrorInvalidType x
    checkT _ _ = return ()

    checkI :: Interface -> IO ()
    checkI iface = forM_ (M.toList iface) aux
      where
        aux (tag, ts) = forM_ ts (checkT (showWithPos tag))

    checkIs :: InterfaceEnv -> IO ()
    checkIs ienv = forM_ (M.elems ienv) checkI

    checkPs :: ProcessDefinitions -> IO ()
    checkPs ps = forM_ (M.elems ps) aux
      where
        aux (xs, p) = checkNs xs >> checkP p
