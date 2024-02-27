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

module Inference (generate) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import qualified Control.Monad.State.Lazy as ST
import Control.Exception
import Control.Monad (forM_, unless)
import Debug.Trace (trace, traceM)

import Aux
import Language
import Pattern
import Exceptions
import qualified Dependencies as G

data InferenceState
  = InferenceState {
    next :: Int,
    cset :: ConstraintSet
  }

type Inference a = ST.State InferenceState a
type ProcessEnv = M.Map PName [Type]
type Σ = M.Map Name IName
type Γ = M.Map Name Type

----------------------
-- MONAD OPERATIONS --
----------------------

newPattern :: Inference Pattern
newPattern = do
  state <- ST.get
  let n = next state
  ST.put (state { next = n + 1 })
  return $ PVar n

addConstraint :: Pattern -> Pattern -> Inference ()
addConstraint π₁ π₂ | π₁ == π₂ = return ()
addConstraint π₁ π₂ = do
  state <- ST.get
  ST.put (state { cset = S.insert (π₁ :⊑: π₂) (cset state) })

-----------------------
-- LOOKUP OPERATIONS --
-----------------------

lookupInterface :: InterfaceEnv -> IName -> Interface
lookupInterface ienv iname =
  case M.lookup iname ienv of
    Just iface -> iface
    Nothing -> throw $ ErrorUnknownIdentifier "interface" (showWithPos iname)

lookupProcess :: ProcessEnv -> PName -> [Type]
lookupProcess penv pname =
  case M.lookup pname penv of
    Just ts -> ts
    Nothing -> throw $ ErrorUnknownIdentifier "process" (showWithPos pname)

lookupName :: Σ -> Name -> IName
lookupName σ u =
  case M.lookup u σ of
    Just iname -> iname
    Nothing -> throw $ ErrorUnknownIdentifier "name" (showWithPos u)

lookupMessage :: InterfaceEnv -> IName -> Tag -> [Type]
lookupMessage ienv iname tag =
  case M.lookup tag (lookupInterface ienv iname) of
    Just ts -> ts
    Nothing -> throw $ ErrorUnknownIdentifier "message" (showWithPos tag)

typeOfName :: Γ -> Name -> Inference Type
typeOfName γ u =
  case M.lookup u γ of
    Just t -> return t
    Nothing -> throw $ ErrorUnknownIdentifier "name" (showWithPos u)

----------------------------
-- ENVIRONMENT OPERATIONS --
----------------------------

joinEnvironments :: Γ -> Γ -> Inference Γ
joinEnvironments = combineEnvironments aux
  where
    aux :: Name -> Type -> Name -> Type -> Inference Type
    aux _ None _ t = return t
    aux _ t _ None = return t
    aux u₁ (Type iname₁ _ _) u₂ (Type iname₂ _ _) | iname₁ /= iname₂
      = error $ "joinEnvironments: this should not happen: " ++ showWithPos u₁ ++ " and " ++ showWithPos u₂
    aux _ (Type iname Out π₁) _ (Type _ Out π₂) = return $ Type iname Out (π₁ :·: π₂)
    aux _ (Type iname Out π₁) _ (Type _ In π₂) = do
      π <- newPattern
      addConstraint (π₁ :·: π) π₂
      return $ Type iname In π
    aux u₁ t₁@(Type _ In _) u₂ t₂@(Type _ Out _) = aux u₂ t₂ u₁ t₁
    aux u₁ _ u₂ _ = throw $ ErrorTypeMismatch ("non-linear ? capability: " ++ showWithPos u₁ ++ " and " ++ showWithPos u₂)

mergeEnvironments :: Γ -> Γ -> Inference Γ
mergeEnvironments = combineEnvironments aux
  where
    aux :: Name -> Type -> Name -> Type -> Inference Type
    aux _ None _ mt = return mt
    aux _ mt _ None = return mt
    aux u₁ (Type iname₁ _ _) u₂ (Type iname₂ _ _) | iname₁ /= iname₂
      = error "mergeEnvironments: this should not happen"
    aux _ (Type iname Out π₁) _ (Type _ Out π₂) = return $ Type iname Out (π₁ :+: π₂)
    aux _ (Type iname In π₁) _ (Type _ In π₂) = do
      π <- newPattern
      addConstraint π π₁
      addConstraint π π₂
      return (Type iname In π)
    aux u₁ t₁ u₂ t₂ = throw $ ErrorTypeMismatch ("unmergeable types: " ++ showWithPos u₁ ++ " : " ++ show t₁ ++ " and " ++ showWithPos u₂ ++ " : " ++ show t₂)

combineEnvironments :: (Name -> Type -> Name -> Type -> Inference Type) -> Γ -> Γ -> Inference Γ
combineEnvironments combineTypes γ₁ γ₂ = do
  let ut₁ = M.toAscList γ₁
  let ut₂ = M.toAscList γ₂
  assert (length ut₁ == length ut₂) (return ())
  ut <- mapM (uncurry aux) (zip ut₁ ut₂)
  return $ M.fromList ut
  where
    aux :: (Name, Type) -> (Name, Type) -> Inference (Name, Type)
    aux (u₁, t₁) (u₂, t₂)
      | u₁ == u₂ = do
          t <- combineTypes u₁ t₁ u₂ t₂
          return (u₁, t)
    aux (u₁, _) (u₂, _) = error ("combining " ++ show u₁ ++ " and " ++ show u₂ ++ show γ₁ ++ show γ₂)

fillEnvironment :: Σ -> Γ -> Γ
fillEnvironment σ γ = M.union γ γ'
  where
    γ' = M.map (\iname -> Type iname Out One) (M.difference σ γ)

noneEnvironment :: Σ -> Γ -> Γ
noneEnvironment σ γ = M.union γ $ M.map (const None) (M.difference σ γ)

---------------------
-- TYPE OPERATIONS --
---------------------

interfaceOfType :: Type -> IName
interfaceOfType (Part iname _) = iname
interfaceOfType (Type iname _ _) = iname

replaceInterface :: IName -> Type -> Type
replaceInterface iname (Type _ cap pi) = Type iname cap pi

subtype :: InterfaceEnv -> Type -> Type -> Inference ()
subtype ienv = auxT []
  where
    auxT :: [(IName, IName)] -> Type -> Type -> Inference ()
    auxT _ None _ = return ()
    auxT _ _ None = return ()
    auxT visited (Type iname₁ In π₁) (Type iname₂ In π₂) = do
      auxI visited iname₁ iname₂
      addConstraint π₁ π₂
    auxT visited (Type iname₁ Out π₁) (Type iname₂ Out π₂) = do
      auxI visited iname₂ iname₁
      addConstraint π₂ π₁
    auxT _ t₁ t₂ = throw $ ErrorTypeMismatch $ "expected " ++ show t₂ ++ ", actual " ++ show t₁

    auxI :: [(IName, IName)] -> IName -> IName -> Inference ()
    auxI visited iname₁ iname₂ | (iname₁, iname₂) `elem` visited = return ()
    auxI visited iname₁ iname₂ = do
      let visited' = (iname₁, iname₂) : visited
      let iface = lookupInterface ienv iname₁
      forM_ (M.toList iface) (uncurry (auxM visited' iname₂))

    auxM :: [(IName, IName)] -> IName -> Tag -> [Type] -> Inference ()
    auxM visited iname tag ts = do
      let iface = lookupInterface ienv iname
      case M.lookup tag iface of
        Nothing -> throw $ ErrorTypeMismatch $ "interface mismatch: " ++ show tag ++ " not in " ++ show iname
        Just tsₑ -> do
          unless (length ts == length tsₑ) (throw $ ErrorArityMismatch (identifierPos tag) "message" (length tsₑ) (length ts))
          forM_ (zip ts tsₑ) (uncurry (auxT visited))

---------------------------
-- CONSTRAINT GENERATION --
---------------------------

generate :: Bool -> [S_Interface] -> [S_ProcessDefinition] -> (InterfaceEnv, ProcessDefinitions, ConstraintSet)
generate deadlock idefs ps =
  let initialState = InferenceState { next = 0, cset = S.empty }
      ((ienv, qs), state) = ST.runState auxMain initialState
  in (ienv, qs, cset state)
  where
    auxMain :: Inference (InterfaceEnv, ProcessDefinitions)
    auxMain = do
      ienv <- auxInterfaces idefs
      penv <- auxProcessTypes ienv ps
      qs <- auxProcessDefinitions ienv penv ps
      return (ienv, qs)

    auxInterfaces :: [S_Interface] -> Inference InterfaceEnv
    auxInterfaces idefs = do
      idefs' <- mapM aux idefs
      return $ M.fromListWithKey (throw . ErrorMultipleInterfaceDefinitions) idefs'
        where
          aux (iface, mdefs) = do
            mdefs' <- mapM auxMessageDefinition mdefs
            let menv = M.fromListWithKey (throw . ErrorMultipleMessageDefinitions) mdefs'
            return (iface, menv)

          auxMessageDefinition :: Message -> Inference (Tag, [Type])
          auxMessageDefinition (Message tag ts) = do
            ts' <- mapM auxType ts
            return (tag, ts')

    auxProcessTypes :: InterfaceEnv -> [S_ProcessDefinition] -> Inference ProcessEnv
    auxProcessTypes ienv pdefs = do
      ptypes <- mapM auxProcessType pdefs
      return $ M.fromListWithKey (throw . ErrorMultipleProcessDefinitions) ptypes
      where
        auxProcessType :: (PName, [(Name, Type)], S_Process) -> Inference (PName, [Type])
        auxProcessType (pname, targs, _) = do
          ts <- mapM (auxType . snd) targs
          return (pname, ts)

    auxType :: Type -> Inference Type
    auxType (Type iname cap π) = return $ Type iname cap π
    auxType (Part iname cap) = do
      π <- newPattern
      return $ Type iname cap π

    auxProcessDefinitions :: InterfaceEnv -> ProcessEnv -> [S_ProcessDefinition] -> Inference ProcessDefinitions
    auxProcessDefinitions ienv penv pdefs = do
      pdefs' <- mapM (auxProcessDefinition ienv penv) pdefs
      return $ M.fromList pdefs'

    auxProcessDefinition :: InterfaceEnv -> ProcessEnv ->
      (PName, [(Name, Type)], S_Process) -> Inference (PName, ([O_Name], O_Process))
    auxProcessDefinition ienv penv (pname, xts, p) = do
      let xs = map fst xts
      let tsₑ = lookupProcess penv pname
      unless (length xs == length tsₑ) (throw $ ErrorArityMismatch (identifierPos pname) "process" (length tsₑ) (length xs))
      let inames = map interfaceOfType tsₑ
      let σ = M.fromListWithKey (throw . ErrorMultipleNameDeclarations) (zip xs inames)
      (q, γ, _) <- auxProcess ienv penv σ p
      ts <- mapM (typeOfName γ) xs
      forM_ (zip tsₑ ts) (uncurry (subtype ienv))
      return (pname, (zip xs tsₑ, q))

    auxArgument :: (Name, Type) -> Inference (Name, Type)
    auxArgument (u, t) = do
      t' <- auxType t
      return (u, t')

    auxProcess :: InterfaceEnv -> ProcessEnv -> Σ -> S_Process -> Inference (O_Process, Γ, G.DependencyGraph)
    auxProcess _ _ σ Done = do
      let γ = M.empty
      return (Done, fillEnvironment σ γ, G.empty)
    auxProcess ienv _ σ (Output u tag vs) = do
      let iname = lookupName σ u
      let inames = map (lookupName σ) vs
      let tsₑ = lookupMessage ienv iname tag
      unless (length vs == length tsₑ) (throw $ ErrorArityMismatch (identifierPos tag) "message" (length tsₑ) (length vs))
      let ts = map (uncurry replaceInterface) (zip inames tsₑ)
      forM_ (zip ts tsₑ) (uncurry (subtype ienv))
      let u' = (u, Type iname Out (Atom tag))
      let vs' = zip vs ts
      let γ = M.fromList (u' : vs')
      dg <- checkGraph $ G.fromList (u : vs)
      return (Output u' tag vs', fillEnvironment σ γ, dg)
    auxProcess ienv penv σ (Call pname vs) = do
      let inames = map (lookupName σ) vs
      let tsₑ = lookupProcess penv pname
      unless (length vs == length tsₑ) (throw $ ErrorArityMismatch (identifierPos pname) "process invocation" (length tsₑ) (length vs))
      let ts = map (uncurry replaceInterface) (zip inames tsₑ)
      forM_ (zip ts tsₑ) (uncurry (subtype ienv))
      let vs' = zip vs ts
      let γ = M.fromList vs'
      dg <- checkGraph $ G.fromList vs
      return (Call pname vs', fillEnvironment σ γ, dg)
    auxProcess ienv penv σ (Parallel p₁ p₂) = do
      (q₁, γ₁, dg₁) <- auxProcess ienv penv σ p₁
      (q₂, γ₂, dg₂) <- auxProcess ienv penv σ p₂
      γ <- joinEnvironments γ₁ γ₂
      dg <- checkGraph (G.union dg₁ dg₂)
      return (Parallel q₁ q₂, fillEnvironment σ γ, dg)
    auxProcess ienv penv σ (New u iname p) = do
      let σ' = M.insert u iname σ
      (q, γ', dg) <- auxProcess ienv penv σ' p
      Type _ cap π <- typeOfName γ' u
      let γ = M.delete u γ'
      unless (cap == In) (throw $ ErrorTypeMismatch ("expected ? capability: " ++ showWithPos u))
      addConstraint One π
      let u' = (u, Type iname cap One)
      return (New u' iname q, fillEnvironment σ γ, G.delete u dg)
    auxProcess ienv penv σ (Input u π g) = do
      let iname = lookupName σ u
      (g', γ) <- auxGuard u iname π ienv penv σ g
      Type _ _ π' <- typeOfName γ u
      addConstraint π π'
      let u' = (u, Type iname In π)
      return (Input u' π g', γ, G.fromSet (S.insert u (fng g)))

    auxGuard :: Name -> IName -> Pattern -> InterfaceEnv -> ProcessEnv -> Σ -> S_Guard -> Inference (O_Guard, Γ)
    auxGuard u iname _ _ _ σ (Fail msg) = do
      let γ = M.singleton u (Type iname In Zero)
      return (Fail msg, noneEnvironment σ γ)
    auxGuard u iname _ ienv penv σ (Free p) = do
      let σ' = M.delete u σ
      (q, γ', _) <- auxProcess ienv penv σ' p
      let γ = M.insert u (Type iname In One) γ'
      return (Free q, γ)
    auxGuard u iname π ienv penv σ (Select tag xs p) = do
      let tsₑ = lookupMessage ienv iname tag
      unless (length xs == length tsₑ) (throw $ ErrorArityMismatch (identifierPos tag) "message" (length tsₑ) (length xs))
      let xs' = zip xs tsₑ
      let inames = map interfaceOfType tsₑ
      let σ' = foldr (uncurry M.insert) σ (zip xs inames)
      (q, γ', _) <- auxProcess ienv penv σ' p
      Type _ cap π' <- typeOfName γ' u
      unless (cap == In) (throw $ ErrorTypeMismatch ("expected ? capability: " ++ showWithPos u))
      let πᵣ = Pattern.derivative (Pattern.deriveTag tag) π
      addConstraint πᵣ π'
      ts <- mapM (typeOfName γ') xs
      forM_ (zip tsₑ ts) (uncurry (subtype ienv))
      let γ = M.insert u (Type iname In (Atom tag :·: πᵣ)) (foldr M.delete γ' xs)
      return (Select tag xs' q, fillEnvironment σ γ)
    auxGuard u iname π ienv penv σ (Choice g₁ g₂) = do
      (h₁, γ₁) <- auxGuard u iname π ienv penv σ g₁
      (h₂, γ₂) <- auxGuard u iname π ienv penv σ g₂
      Type _ _ π₁ <- typeOfName γ₁ u
      Type _ _ π₂ <- typeOfName γ₂ u
      γ' <- mergeEnvironments (M.delete u γ₁) (M.delete u γ₂)
      let γ = M.insert u (Type iname In (π₁ :+: π₂)) γ'
      return (Choice h₁ h₂, γ)

    checkGraph :: G.DependencyGraph -> Inference G.DependencyGraph
    checkGraph dg | not deadlock = return dg
    checkGraph dg | G.cyclic dg = throw (ErrorCyclicDependencyGraph (show dg))
                  | otherwise = return dg
