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

module Language where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

---------------
-- POSITIONS --
---------------

data Pos = Somewhere
         | At (Int, Int)

instance Show Pos where
  show Somewhere = ""
  show (At (l, c)) = "@" ++ show l

-------------------------
-- LOCATED IDENTIFIERS --
-------------------------

data ObjectI
data InterfaceI
data ProcessI
data TagI

data Identifier k = Identifier { identifierPos :: Pos
                               , identifierText :: String }
instance Show (Identifier k) where
  show = identifierText

showWithPos :: Identifier k -> String
showWithPos u = identifierText u ++ show (identifierPos u)

instance Eq (Identifier k) where
  (==) u₁ u₂ = identifierText u₁ == identifierText u₂

instance Ord (Identifier k) where
  compare u₁ u₂ = compare (identifierText u₁) (identifierText u₂)

type Name  = Identifier ObjectI
type IName = Identifier InterfaceI
type PName = Identifier ProcessI
type Tag   = Identifier TagI

------------------
-- CAPABILITIES --
------------------

data Capability = In | Out
  deriving (Eq, Ord)

instance Show Capability where
  show In = "?"
  show Out = "!"

--------------
-- PATTERNS --
--------------

type PVar = Int

data Pattern
  = PVar PVar
  | Zero
  | One
  | Atom Tag
  | Pattern :+: Pattern
  | Pattern :·: Pattern
  | Star Pattern
  deriving (Eq, Ord)

type PatternOpt = Maybe Pattern

type PatternMap = M.Map PVar Pattern

----------------
-- INTERFACES --
----------------

data Message = Message Tag [Type]
type S_Interface = (IName, [Message])
type Interface = M.Map Tag [Type]

-----------
-- TYPES --
-----------

data Type = None
          | Part IName Capability
          | Type IName Capability Pattern

--------------
-- LANGUAGE --
--------------

data Process u
  = Done
  | Output u Tag [u]
  | Input u Pattern (Guard u)
  | Parallel (Process u) (Process u)
  | New u IName (Process u)
  | Call PName [u]

data Guard u
  = Fail String
  | Free (Process u)
  | Select Tag [u] (Process u)
  | Choice (Guard u) (Guard u)

type O_Name = (Name, Type)

type S_Process = Process Name
type O_Process = Process O_Name

type S_Guard   = Guard Name
type O_Guard   = Guard O_Name

fng :: Ord u => Guard u -> S.Set u
fng (Fail _) = S.empty
fng (Free p) = fnp p
fng (Select _ xs p) = foldr S.delete (fnp p) xs
fng (Choice g₁ g₂) = S.union (fng g₁) (fng g₂)

fnp :: Ord u => Process u -> S.Set u
fnp Done = S.empty
fnp (Output u _ vs) = S.fromList (u : vs)
fnp (Input u _ g) = S.insert u (fng g)
fnp (Parallel p₁ p₂) = S.union (fnp p₁) (fnp p₂)
fnp (New u _ p) = S.delete u (fnp p)
fnp (Call _ vs) = S.fromList vs

-----------------
-- DEFINITIONS --
-----------------

type S_ProcessDefinition = (PName, [(Name, Type)], S_Process)
type ProcessDefinitions = M.Map PName ([O_Name], O_Process)
type InterfaceEnv = M.Map IName Interface

mapInterface :: (Type -> Type) -> Interface -> Interface
mapInterface f = M.map (map f)

mapProcessDefinitions :: (Type -> Type) -> ProcessDefinitions -> ProcessDefinitions
mapProcessDefinitions f = M.map auxD
  where
    auxD (xs, p) = (map auxN xs, auxP p)

    auxP Done = Done
    auxP (Output u tag vs) = Output (auxN u) tag (map auxN vs)
    auxP (Input u π g) = Input (auxN u) π (auxG g)
    auxP (Parallel p₁ p₂) = Parallel (auxP p₁) (auxP p₂)
    auxP (New u iname p) = New (auxN u) iname (auxP p)
    auxP (Call pname vs) = Call pname (map auxN vs)

    auxG (Fail msg) = Fail msg
    auxG (Free p) = Free (auxP p)
    auxG (Select tag xs p) = Select tag (map auxN xs) (auxP p)
    auxG (Choice g₁ g₂) = Choice (auxG g₁) (auxG g₂)

    auxN (u, t) = (u, f t)

-----------------
-- CONSTRAINTS --
-----------------

data Constraint = Pattern :⊑: Pattern
  deriving (Eq, Ord)

type ConstraintSet = S.Set Constraint
