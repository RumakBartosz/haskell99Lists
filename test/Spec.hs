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

    describe "isPalindrome tests" $ do
      it "returns correct example value" $
        isPalindrome [123, 234, 345] `shouldBe` (False :: Bool)
      it "returns correct value on single element" $
        isPalindrome [1] `shouldBe` (True :: Bool)
      it "works correctly with characters" $
        isPalindrome ['a','b','b','a'] `shouldBe` (True :: Bool)

    describe "compress tests" $ do
      it "returns correct example value" $
        compress [1, 1, 1, 2, 2, 3] `shouldMatchList` [1, 2, 3]
      it "returns correct value on single element" $
        compress [1] `shouldMatchList` [1]
      it "works correctly with characters" $
        compress ['a','b','b','a'] `shouldMatchList` ['a', 'b', 'a']

    describe "flatten tests" $ do
      it "returns correct example value" $
        flatten (List [Elem 1, List [Elem 2, List [Elem 3, Elem 4], Elem 5]]) `shouldMatchList` [1,2,3,4,5]
      it "returns correct value on single element" $
        flatten (Elem 5) `shouldMatchList` [5]
        
    describe "pack tests" $ do
      it "returns correct example value" $
        pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e', 'e']
          `shouldMatchList` ["aaaa","b","cc","aa","d","eeee"]
      it "returns correct example on non change" $
        pack ['a','b','a'] `shouldMatchList` ["a","b","a"]