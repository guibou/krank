import Test.Hspec

main :: IO ()
main = hspec $ do
  describe "first test" $ do
    it "works" $ do
      (1 :: Int) `shouldBe` 1
