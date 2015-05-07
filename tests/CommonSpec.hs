module CommonSpec where

import Test.Hspec
import Utils (unsafeCatchException)

import OhBool.Common


spec :: Spec
spec = do
  describe "Common" $ do
    it "show var" $ do
      show (Var 'x') `shouldBe` "Var 'x'"
    it "eq var" $ do
      Var 'x' == Var 'x'
