{-# LANGUAGE FlexibleContexts #-}

--------------------------------------------------------------------------------
-- | This module contains the code for converting an `Expr` to a "A-Normal" form.
--------------------------------------------------------------------------------
module Language.Diamondback.Checker
  ( -- * Top-level Static Checker
    check

    -- * Error Constructors
  , errUnboundVar
  , errUnboundFun
  ) where

import           Control.Exception
import           Data.Monoid
import qualified Data.List          as L
import           Language.Diamondback.Types
import           Language.Diamondback.Utils

--------------------------------------------------------------------------------
check :: BareProgram -> BareProgram
--------------------------------------------------------------------------------
check p = case wellFormed p of
            [] -> p
            es -> throw es

-- | Map from function name to arity
type FunEnv = Env

--------------------------------------------------------------------------------
-- | `wellFormed p` returns the list of errors for a program `p`
--------------------------------------------------------------------------------
wellFormed :: BareProgram -> [UserError]
--------------------------------------------------------------------------------
wellFormed (Prog ds e) = duplicateFunErrors ds
                      ++ concatMap (wellFormedD fEnv) ds
                      ++ wellFormedE fEnv emptyEnv e
  where
    fEnv               = fromListEnv [(bindId f, length xs) | Decl f xs _ _ <- ds]

--------------------------------------------------------------------------------
-- | `wellFormedD fEnv vEnv d` returns the list of errors for a func-decl `d`
--------------------------------------------------------------------------------
wellFormedD :: FunEnv -> BareDecl -> [UserError]
wellFormedD fEnv (Decl _ xs e _) = if length l /= 0 
                                    then [errDupParam (head l)] else []
                                    ++ wellFormedE fEnv vEnv e
  where
    vEnv                         = foldr addEnv emptyEnv xs
    l                            = concat (dupBy bindId xs)

--------------------------------------------------------------------------------
-- | `wellFormedE vEnv e` returns the list of errors for an expression `e`
--------------------------------------------------------------------------------
wellFormedE :: FunEnv -> Env -> Bare -> [UserError]
wellFormedE fEnv env e = go env e
  where
  gos env es               = concatMap (go env) es
  go _   (Boolean {})      = []
  go _   (Number  n     l) = if n >= maxInt || n < -maxInt then [errLargeNum l n] else []
  go env (Id      x     l) = unboundVarErrors env x l
  go env (Prim1 _ e     _) = go  env e
  go env (Prim2 _ e1 e2 _) = gos env [e1, e2]
  go env (If   e1 e2 e3 _) = gos env [e1, e2, e3]
  go env (Let x e1 e2   _) = go env e1
                           ++ msg
                           ++ go (addEnv x env) e2
    where
      msg = case (lookupEnv (bindId x) env) of
        Just a   -> [errDupBind x]
        Nothing  -> []
  go env (App f es      l) = funErrors fEnv f es l 
                           ++ gos env es


--------------------------------------------------------------------------------
-- | Error Checkers: In each case, return an empty list if no errors.
--------------------------------------------------------------------------------
duplicateFunErrors :: [BareDecl] -> [UserError]
duplicateFunErrors
  = fmap errDupFun
  . concat
  . dupBy (bindId . fName)

unboundVarErrors :: Env -> Id -> SourceSpan -> [UserError] 
unboundVarErrors env x l  = case (lookupEnv x env) of
      Just a   -> []
      Nothing  -> [errUnboundVar l x]


funErrors fEnv f es l = case (lookupEnv f fEnv) of
      Just a   -> if length es == a then []
                  else [errCallArity l f]
      Nothing  -> [errUnboundFun l f]

-- | `maxInt` is the largest number you can represent with 31 bits (accounting for sign
--    and the tag bit.

maxInt :: Integer
maxInt = 1073741823

--------------------------------------------------------------------------------
-- | Error Constructors: Use these functions to construct `UserError` values
--   when the corresponding situation arises. e.g see how `errDupFun` is used.
--------------------------------------------------------------------------------

errDupFun :: (Located (Bind a)) => Decl a -> UserError
errDupFun d = mkError "duplicate function" (sourceSpan f) where f = fName d

errDupParam :: (Located (Bind a)) => Bind a -> UserError
errDupParam x = mkError "duplicate parameter" (sourceSpan x)

errDupBind :: (Located (Bind a)) => Bind a -> UserError
errDupBind x = mkError "shadow binding" (sourceSpan x)

errLargeNum :: SourceSpan -> Integer -> UserError
errLargeNum   l _n = mkError "too large" l

errUnboundVar :: SourceSpan -> Id -> UserError
errUnboundVar l _x = mkError "unbound variable" l

errUnboundFun :: SourceSpan -> Id -> UserError
errUnboundFun l _f = mkError "not defined" l

errCallArity :: SourceSpan -> Id -> UserError
errCallArity  l _f = mkError "arity" l
