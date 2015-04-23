module OhBool.Evaluation where

import OhBool.Common

import Control.Monad (liftM)
import Data.Bits ((.|.), (.&.), xor)

evaluateAt :: Vars -> Expression -> Maybe Bool
evaluateAt vars expr = case expr of
  BinaryExpression op ex1 ex2 -> do
    r1 <- evaluateAt vars ex1
    r2 <- evaluateAt vars ex2
    return $ performOperation op r1 r2
  Not ex ->
    liftM not (evaluateAt vars ex)
  Variable v ->
    getValue v vars

performOperation :: BinaryOperator -> Bool -> Bool -> Bool
performOperation Or  = (.|.)
performOperation And = (.&.)
performOperation Xor = xor
