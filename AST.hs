module AST where

{-
(|>) :: a -> (a -> b) -> b
(|>) x f = f x
-}

{- Syntax of Types -}
data Qualifier = Linear | Unlimited
  deriving (Show)

type TyVar = String

data Ty =
    TyBool
  | TyEnd
  | TyQual Qualifier Pretype
  deriving (Show)

data Pretype =
    TyRecv Ty Ty -- Receive Type Continuation
  | TySend Ty Ty -- Send Type Continuation
  | TyBranch [(Label, Ty)]
  | TySelect [(Label, Ty)]
  deriving (Show)


type Context = [(TyVar, Ty)]

{- Syntax of Terms -}
data Channel = Ch String -- Name of channel
  deriving (Show)

data Value =
    Variable String
  | VBool Bool
  deriving (Show)

data Label = Lbl String
  deriving (Show)

data Process =
    Output Channel Value Process -- Output value over channel, continue as Process
    -- Input value over channel, qualifier allows replication
  | Input Qualifier Channel Value Process
  | Par Process Process -- Parallel Composition
  | If Value Process Process -- Conditional
  | Inaction -- End
  -- Binary scope restriction, with type annotation
  | ScopeRestriction Channel Channel Ty Process
  | Select Label Process
  | Branch [(Label, Process)]
  deriving (Show)

