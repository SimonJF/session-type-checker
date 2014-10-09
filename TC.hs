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
contextDiff :: TCEnv -> TCEnv -> TCEnv
contextDiff e1 [] = e1
contextDiff _ _ = undefined


{- type TC a = ExceptT String (State TCState) a -}
type TC a = TCState -> Either String a

err = Left


-- Duality function on types. Partial!
tyDual :: Ty -> Either String Ty
tyDual (TyQual q pt) = Right $ TyQual q (preTyDual pt)
  where preTyDual :: Pretype -> Pretype
        preTyDual (TyRecv t1 t2) = TySend t1 (tyDual t2)
        preTyDual (TySend t1 t2) = TyRecv t1 (tyDual t2)
        preTyDual (TySelect xs) = TyBranch (map (\ (l, t) -> (l, (tyDual t))) xs)
        preTyDual (TyBranch xs) = TySelect (map (\ (l, t) -> (l, (tyDual t))) xs)
tyDual TyEnd = Right TyEnd
tyDual t = Left $ "Duality undefined for type " ++ (show t)


typeCheckVal :: Value -> TC (Ty, TCState)
typeCheckVal (VBool _) st = TyBool
typeCheckVal (Variable name) st =
    case lookup name (tcEnv st) of
      Just ty ->
        if isUnlimitedType ty then
          (ty, st)
        else
          (ty, (consume name st))
      Nothing -> err $ "Unbound type variable " ++ name

typeCheck :: Process -> TC Ty
typeCheck Inaction = return TyEnd
-- typeCheck 
