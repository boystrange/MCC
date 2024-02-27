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

module Dependencies
  ( DependencyGraph
  , empty
  , cyclic
  , fromList
  , fromSet
  , delete
  , union )
where

import qualified Data.Set as S
import qualified Data.Partition as P

import Language

data DependencyGraph
  = Cyclic Name Name Name Name
  | Acyclic (P.Partition Name)

instance Show DependencyGraph where
  show (Cyclic x₁ y₁ x₂ y₂) = "cycle " ++ showWithPos x₁ ++ " → " ++ showWithPos y₁ ++ " = " ++ showWithPos y₂ ++ " → " ++ showWithPos x₂
  show (Acyclic p) = show (edges p)

empty :: DependencyGraph
empty = Acyclic P.empty

cyclic :: DependencyGraph -> Bool
cyclic (Acyclic _) = False
cyclic (Cyclic _ _ _ _) = True

fromList :: [Name] -> DependencyGraph
fromList xs = fromEdges (zip xs (tail xs))

fromSet :: S.Set Name -> DependencyGraph
fromSet = fromList . S.elems

fromEdges :: [(Name, Name)] -> DependencyGraph
fromEdges = addEdges empty

addEdges :: DependencyGraph -> [(Name, Name)] -> DependencyGraph
addEdges = foldr aux
  where
    aux _ g@(Cyclic _ _ _ _) = g
    aux (x, y) (Acyclic p) | P.find p x == P.find p y = Cyclic (remap p x) (remap p y) x y
                           | otherwise = Acyclic (P.joinElems x y p)

delete :: Name -> DependencyGraph -> DependencyGraph
delete _ g@(Cyclic _ _ _ _) = g
delete x (Acyclic p) = Acyclic (P.fromDisjointSets $ map (S.delete x) $ P.nontrivialSets p)

union :: DependencyGraph -> DependencyGraph -> DependencyGraph
union _ g@(Cyclic _ _ _ _) = g
union g (Acyclic p₂) = addEdges g (edges p₂)

edges :: P.Partition Name -> [(Name, Name)]
edges = concat . map ((\xs -> zip xs (tail xs)) . S.toList) . P.nontrivialSets

remap :: P.Partition Name -> Name -> Name
remap p x = S.findMin $ S.filter (== x) $ P.find p x
