{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OhBool.Common where

import qualified Data.Map as M

data BinaryOperator = Or
                    | And
                    | Xor
                    deriving (Show, Eq)

type VarValue = Bool
newtype Var = Var Char deriving (Show,Ord,Eq)
type Vars = M.Map Var VarValue

data TruthTable = TruthTable { expression :: Expression
                             , evaluation :: M.Map Vars Bool } deriving Eq

instance Show TruthTable where
  show (TruthTable expr xs) = "Truth table for " ++ show expr ++ "\n" ++ (show xs)

getValue :: Var -> Vars -> Maybe VarValue
getValue = M.lookup

data Expression = BinaryExpression BinaryOperator Expression Expression
                | Not Expression
                | BoolChain BinaryOperator [Expression]
                | BoolValue Bool
                | Variable Var
                deriving (Show, Eq)
