import Test.Hspec
import Lib

main :: IO ()
main = hspec $
  describe "elementAt tests" $ do
    it "returns correct example value" $
      elementAt [1,2,3,4,5] 3 `shouldBe` (3 :: Int)
    it "returns correct value on single element" $
      elementAt [1] 1 `shouldBe` (1 :: Int)
    it "works correctly with characters" $
      elementAt ['a', 'z', 'x', 'c'] 2 `shouldBe` ('z' :: Char)
