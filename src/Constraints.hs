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

module Constraints where

import qualified Data.Set as S
import qualified Data.Map.Strict as M

import Language
import Exceptions
import qualified Presburger.Formula as P
import qualified Pattern
import Control.Exception
import Control.Monad (forM_, unless, when)

variables :: Constraint -> S.Set PVar
variables (π₁ :⊑: π₂) = S.union (Pattern.variables π₁) (Pattern.variables π₂)

resolve :: (P.Goal -> IO Bool) -- ^ the solver of subtype relations
        -> Bool                -- ^ verbose?
        -> ConstraintSet       -- ^ the whole set of constraints
        -> IO PatternMap
resolve solver verbose cset = do
  unless (S.size cset == length lowerBounds + length upperBounds)
    (error "unexpected constraint")
  forM_ upperBounds checkConstraint
  return solution
  where
    vars :: S.Set PVar
    vars = S.unions (map variables (S.elems cset))

    boundedVars :: S.Set PVar
    boundedVars = S.fromList [ α | _ :⊑: PVar α <- S.elems cset ]

    unboundedVars :: S.Set PVar
    unboundedVars = S.difference vars boundedVars

    checkConstraint c@(π₁ :⊑: π₂) = do
      when verbose (putStrLn $ show c)
      res <- solver (Pattern.sub eπ₁ eπ₂)
      unless res (throw $ ErrorTypeMismatch $ show eπ₁ ++ " ⋢ " ++ show eπ₂)
        where
          eπ₁ = mapPattern π₁
          eπ₂ = mapPattern π₂

    mapPattern :: Pattern -> Pattern
    mapPattern = Pattern.substAll solution

    lowerBounds :: [Constraint]
    lowerBounds = [ c | c@(_ :⊑: PVar _) <- S.elems cset ]

    upperBounds :: [Constraint]
    upperBounds = [ c | c@(_ :⊑: π) <- S.elems cset, Pattern.defined π ]

    equations :: [(PVar, Pattern)]
    equations = [ (α, π) | π :⊑: PVar α <- lowerBounds ] ++ [ (α, Zero) | α <- S.elems unboundedVars ]

    system :: PatternMap
    system = M.fromListWith (:+:) equations

    -- phase 1: we resolve each equation from top to bottom and
    -- substitute as we move forward
    solution₁ :: PatternMap
    solution₁ =
      foldr (\(α, π) m ->
                let π' = Pattern.resolve α (Pattern.substAll m π)
                in M.insert α π' m) M.empty (M.toAscList system)

    -- phase 2: we substitute each equation from bottom to top
    solution₂ :: PatternMap
    solution₂ =
      foldr (\(α, π) m ->
                let π' = Pattern.substAll m π
                in M.insert α π' m) M.empty (M.toDescList solution₁)

    -- the solution is the simplification of phase 2
    solution :: PatternMap
    solution = M.map Pattern.simplify solution₂
