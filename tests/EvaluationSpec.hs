module EvaluationSpec where

import Control.Monad.Reader
import qualified Data.Map as M

import Test.Hspec
import Test.Hspec.Expectations
import Test.Hspec.QuickCheck

import OhBool.Evaluation
import OhBool.Common

import Utils

spec :: Spec
spec = do
    describe "Evaluation" $ do
        it "evaluates the xor" $ do
            performOperation Xor True False `shouldBe` True

        it "evaluates the test expr" $ do
          result someExpr `shouldBe` True

        it "evaluates the 2nd test expr" $ do
          result someExpr2 `shouldBe` False

        it "evaluates the 3rd test expr" $ do
          result someExpr3 `shouldBe` True

        it "evaluates the 4th test expr" $ do
          unsafeCatchException (result someExpr4) `shouldBe` Nothing

        it "generates a TruthTable for expr1" $ do
          constructTruthTable onlyVar `shouldBe` TruthTable onlyVar (M.fromList [(M.fromList [(Var 'x',False)],False),(M.fromList [(Var 'x',True)],True)])

        it "prints something for the TruthTable" $ do
          length (show $ constructTruthTable onlyVar) > 0

        it "does not equal two different expressions" $ do
          someExpr /= someExpr2

someExpr :: Expression
someExpr = Not (BinaryExpression Xor (Variable $ Var 'x') (Variable $ Var 'y'))

someExpr2 :: Expression
someExpr2 = Not (BoolChain Or [(BoolValue False),(BinaryExpression And (BoolValue True) (BoolValue True))])

someExpr3 :: Expression
someExpr3 = Not (BoolChain And [(BoolValue True),(BoolValue False)])

someExpr4 :: Expression
someExpr4 = Variable $ Var '.'

onlyVar :: Expression
onlyVar = BinaryExpression Or (Not (BoolValue True)) (BoolChain And [Variable $ Var 'x'])

vars :: Vars
vars = M.fromList [(Var 'x', True), (Var 'y', True)]

result :: Expression -> Bool
result e = runReader (evaluate e) vars
