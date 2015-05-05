{-# LANGUAGE FlexibleContexts #-}
module OhBool.Evaluation where

import OhBool.Common

import qualified Data.Map as M
import Control.Monad.State.Class
import Control.Monad (liftM)
import Data.Bits ((.|.), (.&.), xor)

{-| Expression tree transformed into a State -}
evaluate :: (MonadState Vars m) => Expression -> m Bool
evaluate (Not ex) = liftM not (evaluate ex)
evaluate (BinaryExpression op ex1 ex2) = do
  r1 <- evaluate ex1
  r2 <- evaluate ex2
  return $ performOperation op r1 r2
evaluate (BoolValue b) = return b
evaluate (BoolChain op exprs) = do
  values <- mapM evaluate exprs
  return $ foldl1 (performOperation op) values
evaluate (Variable v) = do
  vars <- get
  case getValue v vars of
    Just b -> return b
    Nothing -> error $ "Variable not found " ++ show v

{-| Get the concrete binary operator corresponding to the function -}
performOperation :: BinaryOperator -> Bool -> Bool -> Bool
performOperation Or  = (.|.)
performOperation And = (.&.)
performOperation Xor = xor
