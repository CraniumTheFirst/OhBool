{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module OhBool.Common where

import qualified Data.Map as M
import Data.Ord (compare,comparing)
import Data.List (maximumBy, intercalate)
import Data.Function (on)

data BinaryOperator = Or
                    | And
                    | Xor
                    deriving (Eq)

instance Show BinaryOperator where
  show Or = "⋁"
  show And = "⋀"
  show Xor = "⊕"

type VarValue = Bool --T | F | Und deriving (Show, Eq)
newtype Var = Var Char deriving (Ord,Eq)

instance Show Var where
  show (Var c) = c:""

type Vars = M.Map Var VarValue

data TruthTable = TruthTable { expression :: Expression
                             , evaluation :: M.Map Vars VarValue } deriving Eq


instance Show TruthTable where
  show (TruthTable expr xs) = "Truth table for " ++ show expr ++ "\n" ++ (show xs)

getValue :: Var -> Vars -> Maybe VarValue
getValue = M.lookup

data Expression = BinaryExpression BinaryOperator Expression Expression
                | Not Expression
                | BoolChain BinaryOperator [Expression]
                | BoolValue Bool
                | Variable Var
                deriving (Eq)

precedenceOp :: BinaryOperator -> Int
precedenceOp Or = 2
precedenceOp Xor = 2
precedenceOp And = 3

precedence :: Expression -> Int
precedence (BoolValue _) = 5
precedence (Variable _) = 5
precedence (Not _) = 4
precedence (BinaryExpression op _ _) = precedenceOp op
precedence (BoolChain op _) = precedenceOp op

needParentheses :: Expression -> Expression -> Bool
needParentheses parent expr = precedence parent > precedence expr

addParentheses :: String -> String
addParentheses s = "(" ++ s ++ ")"

exprPrettyPrint :: Expression -> String
exprPrettyPrint (Variable x) = show x
exprPrettyPrint expr@(Not ex) = "~" ++ ex' ++ ""
  where ex' = (if needParentheses expr ex then addParentheses else id) $ show ex
exprPrettyPrint (BoolValue b) = show b
exprPrettyPrint expr@(BinaryExpression op exp1 exp2) = parenthify exp1 ++ " " ++ show op ++ " " ++ parenthify exp2
  where parenthify e = (if needParentheses expr e then addParentheses else id) $ show e
exprPrettyPrint expr@(BoolChain op exprs) = intercalate (" " ++ show op ++ " ") $ map parenthify $ exprs
  where parenthify e = (if needParentheses expr e then addParentheses else id) $ show e

instance Show Expression where
  show = exprPrettyPrint
