module TC where

import AST
-- import Control.Monad.State
-- import Control.Monad.Trans.Except

type TCEnv = [(TyVar, Ty)]

data TCState = TCSt
  { tcEnv :: TCEnv, tcFreeLinVars :: [TyVar] }



isUnlimitedType :: Ty -> Bool
isUnlimitedType (TyQual Linear _) = False
isUnlimitedType _ = True

-- Consume a linear variable
consume :: TyVar -> TCState -> TCState
consume tv st =
  let env' = filter (\(n, _) -> n /= tv) (tcEnv st) in
    st { tcEnv = env' }

-- Context Difference Operation
contextDiff :: TCEnv -> [TyVar] -> TCEnv
contextDiff e1 [] = e1
contextDiff e1 e2 = foldr (\x acc -> removeVar x acc) e1 e2
  where removeVar :: TyVar -> TCEnv -> TCEnv
        removeVar _ [] = []
        removeVar v ((n, t) : xs)
          | v == n =
            if isUnlimitedType t then xs
            else error $ "Linear type " ++ v ++ " in context diff operation"
          | otherwise = (n, t) : (removeVar v xs)

  -- Check:
  -- * That the variables we are trying to remove are nonlinear
  -- * Something about not being in the domain?
  {- type TC a = ExceptT String (State TCState) a -}
type TC a = TCState -> a

-- Duality function on types. Partial!
tyDual :: Ty -> Ty
tyDual (TyQual q pt) = TyQual q (preTyDual pt)
  where preTyDual :: Pretype -> Pretype
        preTyDual (TyRecv t1 t2) = TySend t1 (tyDual t2)
        preTyDual (TySend t1 t2) = TyRecv t1 (tyDual t2)
        preTyDual (TySelect xs) = TyBranch (map (\ (l, t) -> (l, (tyDual t))) xs)
        preTyDual (TyBranch xs) = TySelect (map (\ (l, t) -> (l, (tyDual t))) xs)
tyDual TyEnd = TyEnd
tyDual t = error $ "Duality undefined for type " ++ (show t)


typeCheckVal :: Value -> TC (Ty, TCState)
typeCheckVal (VBool _) st = (TyBool, st)
typeCheckVal (Variable name) st =
    case lookup name (tcEnv st) of
      Just ty ->
        if isUnlimitedType ty then
          (ty, st)
        else
          (ty, (consume name st))
      Nothing -> error $ "Unbound type variable " ++ name

typeCheck :: Process -> TC Ty
typeCheck Inaction _ = TyEnd
-- typeCheck
