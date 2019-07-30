import Test.Hspec
import Lib

main :: IO ()
main = hspec $
  describe "all functions tests" $ do
    describe "elementAt tests" $ do
      it "returns correct example value" $
        elementAt [1,2,3,4,5] 3 `shouldBe` (3 :: Int)
      it "returns correct value on single element" $
        elementAt [1] 1 `shouldBe` (1 :: Int)
      it "works correctly with characters" $
        elementAt ['a', 'z', 'x', 'c'] 2 `shouldBe` ('z' :: Char)

    describe "myLength tests" $ do
      it "returns correct example value" $
        myLength [123, 234, 345] `shouldBe` (3 :: Int)
      it "returns correct value on single element" $
        myLength [1] `shouldBe` (1 :: Int)
      it "works correctly with characters" $
        myLength ['a','b','z','k'] `shouldBe` (4 :: Int)


    describe "myReverse tests" $ do
      it "returns correct example value" $
        myReverse [123, 234, 345] `shouldMatchList` [345, 234, 123]
      it "returns correct value on single element" $
        myReverse [1] `shouldMatchList` [1]
      it "works correctly with characters" $
        myReverse ['a','b','z','k'] `shouldMatchList` ['k','z','b','a']

