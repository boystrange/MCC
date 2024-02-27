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

module Pattern
  ( derivative
  , deriveTag
  , deriveVar
  , resolve
  , variables
  , defined
  , zero
  , simplify
  , substAll
  , sub )
where

import qualified Data.Set as S
import qualified MultiSet as MS
import qualified Data.Map.Strict as M

import Language
import Render
import qualified Presburger.Formula as P

signature :: Pattern -> S.Set Tag
signature Zero = S.empty
signature One = S.empty
signature (PVar n) = error $ "signature of " ++ showPatternVariable n ++ " is undefined"
signature (Atom tag) = S.singleton tag
signature (π₁ :·: π₂) = S.union (signature π₁) (signature π₂)
signature (π₁ :+: π₂) = S.union (signature π₁) (signature π₂)
signature (Star π) = signature π

variables :: Pattern -> S.Set PVar
variables Zero = S.empty
variables One = S.empty
variables (PVar a) = S.singleton a
variables (Atom _) = S.empty
variables (π₁ :+: π₂) = S.union (variables π₁) (variables π₂)
variables (π₁ :·: π₂) = S.union (variables π₁) (variables π₂)
variables (Star π) = variables π

-- guiding principle: no operator is distributed
simplify :: Pattern -> Pattern
simplify = aux
  where
    aux Zero = Zero
    aux One = One
    aux π@(PVar _) = π
    aux π@(Atom _) = π
    aux (π₁ :+: π₂) =
      case (aux π₁, aux π₂) of
        (Zero, π) -> π
        (π, Zero) -> π
        (sπ₁, sπ₂) | sπ₁ == sπ₂ -> sπ₁
        (sπ₁, sπ₂) -> sπ₁ :+: sπ₂
    aux (π₁ :·: π₂) =
      case (aux π₁, aux π₂) of
        (Zero, _) -> Zero
        (_, Zero) -> Zero
        (One, π) -> π
        (π, One) -> π
        (sπ₁, sπ₂) -> sπ₁ :·: sπ₂
    aux (Star π) =
      case aux π of
        Zero -> One
        One -> One
        sπ -> Star sπ

zero :: Pattern -> Bool
zero Zero = True
zero One = False
zero (Atom _) = False
zero (π₁ :+: π₂) = zero π₁ && zero π₂
zero (π₁ :·: π₂) = zero π₁ || zero π₂
zero (Star _) = False

defined :: Pattern -> Bool
defined = S.null . variables

substAll :: PatternMap -> Pattern -> Pattern
substAll m = aux
  where
    aux (PVar α) | Just π <- M.lookup α m = aux π
    aux (π₁ :·: π₂) = aux π₁ :·: aux π₂
    aux (π₁ :+: π₂) = aux π₁ :+: aux π₂
    aux (Star π) = Star (aux π)
    aux π = π

subst :: PVar -> Pattern -> Pattern -> Pattern
subst α π = substAll (M.singleton α π)

derivative :: (Pattern -> Pattern) -> Pattern -> Pattern
derivative d = aux
  where
    aux (π₁ :·: π₂) = (aux π₁ :·: π₂) :+: (π₁ :·: aux π₂)
    aux (π₁ :+: π₂) = aux π₁ :+: aux π₂
    aux (Star π) = aux π :·: (Star π)
    aux π = d π

deriveTag :: Tag -> Pattern -> Pattern
deriveTag tag (Atom tag') | tag == tag' = One
deriveTag _ _ = Zero

deriveVar :: PVar -> Pattern -> Pattern
deriveVar α (PVar β) | α == β = One
deriveVar _ _ = Zero

resolve :: PVar -> Pattern -> Pattern
resolve α π = subst α π₀ (Star $ derivative (deriveVar α) π) :·: π₀
  where
    π₀ = subst α Zero π

------------------------------
-- PATTERN → SEMILINEAR SET --
------------------------------

type Vector = MS.MultiSet Tag
type Base = Vector
type Period = Vector
type LinearSet = (Base, S.Set Period)
type SemiLinearSet = S.Set LinearSet

normalize :: Pattern -> SemiLinearSet
normalize = aux
  where
    aux Zero = S.empty
    aux One = semiLinearSetOne
    aux (Atom tag) = S.singleton (MS.singleton tag, S.empty)
    aux (t :+: s) = S.union (aux t) (aux s)
    aux (t :·: s) = semiLinearSetProduct (aux t) (aux s)
    aux (Star t) = S.foldr semiLinearSetProduct semiLinearSetOne (S.map linearSetStar (aux t))
    aux t = error $ "cannot normalize " ++ show t

    linearSetProduct :: LinearSet -> LinearSet -> LinearSet
    linearSetProduct (base₁, periods₁) (base₂, periods₂) =
      (MS.union base₁ base₂, S.union periods₁ periods₂)

    semiLinearSetProduct :: SemiLinearSet -> SemiLinearSet -> SemiLinearSet
    semiLinearSetProduct slset₁ slset₂ =
      S.fromList [ linearSetProduct lset₁ lset₂ | lset₁ <- S.toList slset₁, lset₂ <- S.toList slset₂ ]

    linearSetQuasiStar :: LinearSet -> LinearSet
    linearSetQuasiStar (base, periods) = (base, S.union periods (S.singleton base))

    linearSetOne :: LinearSet
    linearSetOne = (MS.empty, S.empty)

    semiLinearSetOne :: SemiLinearSet
    semiLinearSetOne = S.singleton linearSetOne

    linearSetStar :: LinearSet -> SemiLinearSet
    linearSetStar lset = S.insert linearSetOne (S.singleton (linearSetQuasiStar lset))

-----------------------------------------
-- SEMILINEAR SET → PRESBURGER FORMULA --
-----------------------------------------

tupleOfVector :: [Tag] -> Vector -> [Int]
tupleOfVector tags v = map (\tag -> MS.occur tag v) tags

tupleSum :: [P.Expression] -> [P.Expression] -> [P.Expression]
tupleSum tuple₁ tuple₂ = map (uncurry P.Add) (zip tuple₁ tuple₂)

tupleMul :: String -> [Int] -> [P.Expression]
tupleMul x = map (\n -> P.Mul n (P.Var x))

formulaOfLinearSet :: [Tag] -> LinearSet -> P.Formula
formulaOfLinearSet tags (base, periods) = foldr P.Exists conjunction vars
  where
    vars :: [String]
    vars = map (\i -> "$" ++ show i) [1..length periods]

    baseTuple :: [P.Expression]
    baseTuple = map P.Const (tupleOfVector tags base)

    periodsTuples :: [[P.Expression]]
    periodsTuples = map (\(x, vec) -> tupleMul x (tupleOfVector tags vec)) (zip vars (S.elems periods))

    tuple :: [P.Expression]
    tuple = foldl tupleSum baseTuple periodsTuples

    equalities :: [P.Formula]
    equalities = map (\(tag, expr) -> P.Rel P.RelEQ (P.Var $ show tag) expr) (zip tags tuple)

    constraints :: [P.Formula]
    constraints = map (\x -> P.Rel P.RelLE (P.Const 0) (P.Var x)) vars

    conjunction :: P.Formula
    conjunction = foldl P.And (foldl P.And P.TRUE constraints) equalities

formulaOfSemiLinearSet :: [Tag] -> SemiLinearSet -> P.Formula
formulaOfSemiLinearSet tags = S.foldr (P.Or . formulaOfLinearSet tags) P.FALSE

sub :: Pattern -> Pattern -> P.Goal
sub π₁ π₂ = P.Implication (map show tags) f₁ f₂
  where
    sπ₁ = simplify π₁
    sπ₂ = simplify π₂
    f₁ = formulaOfSemiLinearSet tags $ normalize sπ₁
    f₂ = formulaOfSemiLinearSet tags $ normalize sπ₂
    tags = S.elems (S.union (signature sπ₁) (signature sπ₂))
