module TC where

import AST
import Data.List
-- import Control.Monad.State
-- import Control.Monad.Trans.Except

type TCEnv = [(Var, Ty)]

data TCState = TCSt
  { tcEnv :: TCEnv, tcFreeLinVars :: [Var] }
  deriving (Show)


isUnlimitedType :: Ty -> Bool
isUnlimitedType (TyQual Linear _) = False
isUnlimitedType _ = True

-- Consume a linear variable
consume :: Var -> TCState -> TCState
consume tv st =
  let env' = filter (\(n, _) -> n /= tv) (tcEnv st) in
    st { tcEnv = env' }

-- Check if contexts are equivalent
equivContexts :: TCEnv -> TCEnv -> Bool
equivContexts e1 e2 = e1 \\ e2 == []

-- Check if lin var lists are equivalent
equivVars :: [Var] -> [Var] -> Bool
equivVars vs1 vs2 = vs1 \\ vs2 == []

equivState :: TCState -> TCState -> Bool
equivState s1 s2 =
  equivContexts (tcEnv s1) (tcEnv s2) &&
    equivVars (tcFreeLinVars s1) (tcFreeLinVars s2)

-- Context Difference Operation
-- Set difference on the env against a list of tyvars
-- with the caveat that we *cannot* try and remove a linear variable.
contextDiff :: TCEnv -> [Var] -> TCEnv
contextDiff e1 [] = e1
contextDiff e1 e2 = foldr (\x acc -> removeVar x acc) e1 e2
  where removeVar :: Var -> TCEnv -> TCEnv
        removeVar _ [] = []
        removeVar v ((n, t) : xs)
          | v == n =
            if isUnlimitedType t then xs
            else error $ "Linear type " ++ v ++ " in context diff operation"
          | otherwise = (n, t) : (removeVar v xs)

contextUpdate :: TCEnv -> (Var, Ty) -> TCEnv
contextUpdate env (v, t)
  | isUnlimitedType t =
    case lookup v env of
      Just ty ->
        if t == ty then env
        else error $ "Trying to add duplicate entry with different type for var " ++ (show v)
      Nothing ->
        (v, t) : env
  | otherwise = error $ "Trying to add linear variable " ++ (show v) ++ " in context update."

updateEnv :: TCState -> TCEnv -> TCState
updateEnv st env = st { tcEnv = env }

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

typeCheck :: Process -> TC TCState
typeCheck Inaction st = st
typeCheck (Par p1 p2) st =
  -- TC process 1, retrieve resulting state
  let st' = typeCheck p1 st in
  -- Perform context diff, checking to see we're not trying to nuke any free
  -- linear variables
  let diffSt = st { tcEnv = (contextDiff (tcEnv st') (tcFreeLinVars st')) } in
  typeCheck p2 diffSt
typeCheck (If v p1 p2) st =
  let (t, st') = typeCheckVal v st in
  -- Conditional can only be predicated on a bool
  if t /= TyBool then
    error "Non-bool conditional."
  else
    -- Now, we need to type check each process, ensuring that they result in
    -- equivalent states
    let (s1, s2) = ((typeCheck p1 st), (typeCheck p2 st)) in
        if equivState s1 s2 then s1
          else error $ "Branches of conditional with different contexts"
-- Scope restriction: takes 2 channel names, one must have type T and the other
-- must have the dual of this.
-- We TC the process P assuming x is of type T and y is of type ~T, and then
-- the context difference operation ensures that
typeCheck (ScopeRestriction (Ch x) (Ch y) t p) st =
  -- Check whether we can type P with (Gamma, c1 : T, c2 : ~T)
  let st' = st { tcEnv = (x, t) : (y, tyDual t) : (tcEnv st) } in
  let st'' = typeCheck p st' in
  -- Finally, context diff to ensure we've got no nasty linear vars left over
  st'' { tcEnv = contextDiff (tcEnv st') [x, y],
    tcFreeLinVars = (tcFreeLinVars st') \\ [x, y] }
typeCheck t@(Output (Ch x) v p) st =
  -- Three stages:
  -- * Check that we can type x as a send pretype
  -- * Check that the value we're sending is of the right type
  -- * Context update on x, TC continuation
  let (ty, st') = typeCheckVal (Variable x) st in
  case ty of
    TyQual q (TySend outTy contTy) ->
      -- Now TC value against outTy
      let (vTy, st'') = typeCheckVal v st' in
      if vTy /= outTy then
        error $ "Type mismatch when type checking output. Expected type "
          ++ (show outTy) ++ ", got " ++ (show vTy)
      else
        -- Now, finally, context update on st'' with x of type contTy and
        -- TC continuation p: this gets us our final environment.
        -- After this, we need to update the free linear variables list
        -- if x is linear.
        let env' = contextUpdate (tcEnv st'') (x, contTy) in
        let st''' = typeCheck p (updateEnv st'' env') in
        -- Now, update the linear free vars:
        let lvs = if q == Linear then x : (tcFreeLinVars st''') else tcFreeLinVars st''' in
        st''' { tcFreeLinVars = lvs }
    badTy ->
      error $ "Expected send type when typing " ++ (show t) ++ ", got "
        ++ (show badTy)
typeCheck t@(Input q (Ch x) v p) st =
  -- Once again, three stages:
  -- * Check that we can type x as a receive pretype
  -- * Assuming y of type T, ctx update x of type contTy, try and type cont P
  -- * If input qualifier is unrestricted, free linear vars in L should be empty
  let (ty, st') = typeCheckVal (Variable x) st in
  case ty of
    TyQual q2 (TyRecv inTy contTy) ->
      -- Typing P is a tad more complicated: we add y : T to the context (y being the
      -- name of the variable to be bound by the recv), then do context update of
      -- x : U (x being channel name, U being cont type) on this new environment.
      let env = contextUpdate ((v, inTy) : (tcEnv st')) (v, inTy) in
      -- Output:
      --  * Context: Context diff with y
      --  * LinVars: Set difference with y, but if q2 is linear then add x to L
      let env' = contextDiff env [v] in
      let linAdditions = if q2 == Linear then [x] else [] in
      let linVars = linAdditions ++ ((tcFreeLinVars st') \\ [v]) in
      let st'' = TCSt env' linVars in
      -- Finally, we check to see that if qualifier q is unlimited, that the free
      -- linear var set is empty.
      if q == Unlimited then
        if null (tcFreeLinVars st') then
          st''
        else error $ "Error in input: linear quantifier but unconsumed linear variables"
      else st''
    badTy ->
      error $ "Expected recv type when typing " ++ (show t) ++ ", got "
        ++ (show badTy)

