{-# LANGUAGE FlexibleContexts #-}
module OhBool.Evaluation where

import OhBool.Common

import qualified Data.Map as M
import qualified Data.Set as S
import Control.Monad.Reader (runReader)
import Control.Monad.Reader.Class (MonadReader, ask)
import Control.Monad (liftM)
import Data.Bits ((.|.), (.&.), xor)

{-| Expression tree transformed into a State -}
evaluate :: (MonadReader Vars m) => Expression -> m Bool
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
  vars <- ask
  case getValue v vars of
    Just b -> return b
    Nothing -> error $ "Variable not found " ++ show v

{-| Get the concrete binary operator corresponding to the function -}
performOperation :: BinaryOperator -> Bool -> Bool -> Bool
performOperation Or  = (.|.)
performOperation And = (.&.)
performOperation Xor = xor

getVariables' :: Expression -> [Var]
getVariables' (Variable v) = [v]
getVariables' (Not ex) = getVariables' ex
getVariables' (BinaryExpression _ ex1 ex2) = getVariables' ex1 ++ getVariables' ex2
getVariables' (BoolChain _ xs) = concat . map getVariables' $ xs
getVariables' (BoolValue _) = []

getVariables :: Expression -> S.Set Var
getVariables = S.fromList . getVariables'

constructTruthTable :: Expression -> TruthTable
constructTruthTable ex = TruthTable ex eval
  where variables = S.toAscList $ getVariables ex
        states = allPossibilities variables
        eval = M.fromList $ map (\vars -> (vars, runReader (evaluate ex) vars)) states

allPossibilities :: [Var] -> [Vars]
allPossibilities vars = map (\v -> M.fromList v) $ foldl (\ls v -> concatMap (\l -> [(v,False):l,(v,True):l]) ls) [[]] vars
