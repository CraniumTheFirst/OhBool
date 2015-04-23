module EvaluationSpec where

import Test.Hspec
import Test.Hspec.QuickCheck

import OhBool.Evaluation 
import OhBool.Common

spec :: Spec
spec = do
    describe "evaluation" $ do
        it "evaluates the xor" $ do
            performOperation Xor True False `shouldBe` True
