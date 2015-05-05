module EvaluationSpec where

import Control.Monad.State
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

someExpr :: Expression
someExpr = Not (BinaryExpression Xor (Variable $ Var 'x') (Variable $ Var 'y'))

someExpr2 :: Expression
someExpr2 = Not (BoolChain Or [(BoolValue False),(BinaryExpression And (BoolValue True) (BoolValue True))])

someExpr3 :: Expression
someExpr3 = Not (BoolChain And [(BoolValue True),(BoolValue False)])

someExpr4 :: Expression
someExpr4 = Variable $ Var '.'

vars :: Vars
vars = M.fromList [(Var 'x', True), (Var 'y', True)]

result :: Expression -> Bool
result e = fst $ runState (evaluate e) vars
