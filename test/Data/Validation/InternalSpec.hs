module Data.Validation.InternalSpec where

------------------------------------------------------------------------------------------------------------------------
import Test.Hspec
import Data.Map (fromList)
import Language.Haskell.TH
------------------------------------------------------------------------------------------------------------------------
import Data.Validation.Internal
------------------------------------------------------------------------------------------------------------------------

spec :: Spec
spec = parallel $ do

  describe "Semigroup (VCtx f a)" $ do
    it "If applied to two valid contexts, it concatenate the inhabitants." $ do
      ValidCtx [1] <> ValidCtx [2] `shouldBe` (ValidCtx [1,2] :: VCtx String [Int])

    it "If applied to an valid and disputed contexts, it results in the disputed context concatenated with the valid context." $ do
      let
        mkDis = DisputedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      ValidCtx [1] <> (mkDis [2] :: VCtx String [Int]) `shouldBe` mkDis [1,2]

    it "If applied to an valid and refuted contexts, it results in the refuted context." $ do
      let
        r = RefutedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      ValidCtx [1] <> (r :: VCtx String [Int]) `shouldBe` r

    it "If applied to a disputed and valid context, it results in the disputed context concatenated with the valid context." $ do
      let
        mkDis = DisputedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      mkDis [1] <> (ValidCtx [2] :: VCtx String [Int]) `shouldBe` mkDis [1,2]

    it "If applied to two disputed contexts, it concatenates the contexts." $ do
      let
        v1 = DisputedCtx ["Failure1"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"])]) [1]
        v2 = DisputedCtx ["Failure2"] 
          (fromList [([mkName "Field2"], ["Field Failure 2"])]) [2]
        v3 = DisputedCtx ["Failure1", "Failure2"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"]), ([mkName "Field2"], ["Field Failure 2"])]) [1,2]
      (v1 :: VCtx String [Int]) <> v2 `shouldBe` v3

    it "If applied to a disputed and refuted context, it results in the refuted context." $ do
      let
        v1 = DisputedCtx ["Failure1"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"])]) [1]
        v2 = RefutedCtx ["Failure2"] 
          (fromList [([mkName "Field2"], ["Field Failure 2"])])
        v3 = RefutedCtx ["Failure1", "Failure2"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"]), ([mkName "Field2"], ["Field Failure 2"])])
      (v1 :: VCtx String [Int]) <> v2 `shouldBe` v3

    it "If applied to a refuted and valid context, it results in the refuted context." $ do
      let
        r = RefutedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      r <> (ValidCtx [1] :: VCtx String [Int]) `shouldBe` r

    it "If applied to a refuted and disputed context, it concatenates the failures and results in a refuted context." $ do
      let
        v1 = RefutedCtx ["Failure1"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"])])
        v2 = DisputedCtx ["Failure2"] 
          (fromList [([mkName "Field2"], ["Field Failure 2"])]) [2]
        v3 = RefutedCtx ["Failure1", "Failure2"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"]), ([mkName "Field2"], ["Field Failure 2"])])
      (v1 :: VCtx String [Int]) <> v2 `shouldBe` v3

    it "If applied to two refuted contexts, it concatenates the failures." $ do
      let
        v1 = RefutedCtx ["Failure1"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"])])
        v2 = RefutedCtx ["Failure2"] 
          (fromList [([mkName "Field2"], ["Field Failure 2"])])
        v3 = RefutedCtx ["Failure1", "Failure2"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"]), ([mkName "Field2"], ["Field Failure 2"])])
      (v1 :: VCtx String [Int]) <> v2 `shouldBe` v3

  describe "Functor (VCtx f)" $ do
    it "Converts a `VCtx f a` to a `VCtx f b`." $ do
      fmap show (ValidCtx 1 :: VCtx String Int) `shouldBe` ValidCtx "1"

    it "Updates the value in a disputed context while preserving failures." $ do
      let 
        v1 = DisputedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])]) 1
        v2 = DisputedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])]) "1"
      fmap show (v1 :: VCtx String Int) `shouldBe` (v2 :: VCtx String String)

    it "Does not change the contents of an refuted context." $ do
      let v = RefutedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      fmap show (v :: VCtx String Int) `shouldBe` (v :: VCtx String String)

  describe "Applicative (VCtx f)" $ do
    let 
      vac = ValidCtx show :: VCtx String (Int -> String)
      dac = DisputedCtx ["Applicative Failure"] mempty show :: VCtx String (Int -> String)
      rac = RefutedCtx ["Applicative Failure"] mempty :: VCtx String (Int -> String)
      vc = ValidCtx 1 :: VCtx String Int
      dc = DisputedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])]) 1 :: VCtx String Int
      rc = RefutedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])

    it "Constructs a new valid context" $ do
      pure 1 `shouldBe` (ValidCtx 1 :: VCtx String Int)
    
    it "If applied to two valid contexts, it maps the function of the inhabitants." $ do
      vac <*> vc `shouldBe` ValidCtx "1"

    it "If applied to an valid and disputed contexts, it maps the function over the inhabitants while preserving failures." $ do
      vac <*> dc `shouldBe` DisputedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])]) "1"

    it "If applied to an valid and refuted contexts, it results in the refuted context." $ do
      vac <*> rc `shouldBe` (rc :: VCtx String String)

    it "If applied to a disputed and valid context, it maps the function over the inhabitants while preserving failures." $ do
      dac <*> vc `shouldBe` DisputedCtx ["Applicative Failure"] mempty "1"

    it "If applied to two disputed contexts, it maps the function over the inhabitants while concatenating the failures." $ do
      dac <*> dc `shouldBe` DisputedCtx ["Applicative Failure", "Failure"] (fromList [([mkName "Field1"], ["Field Failure"])]) "1"

    it "If applied to a disputed and refuted context, it results in the refuted context." $ do
      dac <*> rc `shouldBe` RefutedCtx ["Applicative Failure", "Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])

    it "If applied to a refuted and valid context, it results in the refuted context." $ do
      rac <*> vc `shouldBe` RefutedCtx ["Applicative Failure"] mempty

    it "If applied to a refuted and disputed context, it concatenates the failures and results in a refuted context." $ do
      rac <*> dc `shouldBe` RefutedCtx ["Applicative Failure", "Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])

    it "If applied to two refuted contexts, it concatenates the failures." $ do
      rac <*> rc `shouldBe` RefutedCtx ["Applicative Failure", "Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])

  describe "Monad (VCtx f)" $ do
    it "Binds a valid context to a function." $ do
      ((ValidCtx 1 :: VCtx String Int) >>= pure . show) `shouldBe` ValidCtx "1"

    it "Binds a disputed context to a function while preserving failures." $ do
      ((DisputedCtx ["Failure"] mempty 1 :: VCtx String Int) >>= pure . show) `shouldBe` DisputedCtx ["Failure"] mempty "1"

    it "Does not change the contents of a refuted context." $ do
      ((RefutedCtx ["Failure"] mempty :: VCtx String Int) >>= pure . show) `shouldBe` RefutedCtx ["Failure"] mempty


  describe "(<!)" $ do
    let
      one = 1 :: Int
      two = 2 :: Int

    it "If applied to two valid contexts, it selects the first." $ do
      ValidCtx [one] <! ValidCtx [two] `shouldBe` (ValidCtx [one] :: VCtx String [Int])

    it "If applied to an valid and disputed contexts, it results in the disputed context using the value of the first." $ do
      let
        mkDis = DisputedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      ValidCtx [one] <! (mkDis [two] :: VCtx String [Int]) `shouldBe` mkDis [one]

    it "If applied to an valid and refuted contexts, it results in the refuted context." $ do
      let
        r = RefutedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      ValidCtx [one] <! (r :: VCtx String [Int]) `shouldBe` r

    it "If applied to a disputed and valid context, it results in the disputed context." $ do
      let
        d = DisputedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])]) [one]
      d <! (ValidCtx [two] :: VCtx String [Int]) `shouldBe` d

    it "If applied to two disputed contexts, it concatenates the failures and selects the value from the first." $ do
      let
        v1 = DisputedCtx ["Failure1"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"])]) [one]
        v2 = DisputedCtx ["Failure2"] 
          (fromList [([mkName "Field2"], ["Field Failure 2"])]) [two]
        v3 = DisputedCtx ["Failure1", "Failure2"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"]), ([mkName "Field2"], ["Field Failure 2"])]) [one]
      v1 <! v2 `shouldBe` v3

    it "If applied to a disputed and refuted context, it results in a refuted context with the failures from both." $ do
      let
        v1 = DisputedCtx ["Failure1"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"])]) [one]
        v2 = RefutedCtx ["Failure2"] 
          (fromList [([mkName "Field2"], ["Field Failure 2"])])
        v3 = RefutedCtx ["Failure1", "Failure2"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"]), ([mkName "Field2"], ["Field Failure 2"])])
      v1 <! v2 `shouldBe` v3

    it "If applied to a refuted and valid context, it results in the refuted context." $ do
      let
        r = RefutedCtx ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])  :: VCtx String String
      r <! (ValidCtx [2] :: VCtx String [Int]) `shouldBe` r

    it "If applied to a refuted and disputed context, it concatenates the failures and results in a refuted context." $ do
      let
        v1 = RefutedCtx ["Failure1"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"])])
        v2 = DisputedCtx ["Failure2"] 
          (fromList [([mkName "Field2"], ["Field Failure 2"])]) [two]
        v3 = RefutedCtx ["Failure1", "Failure2"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"]), ([mkName "Field2"], ["Field Failure 2"])])
      (v1 :: VCtx String [Int]) <! v2 `shouldBe` v3

    it "If applied to two refuted contexts, it concatenates the failures." $ do
      let
        v1 = RefutedCtx ["Failure1"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"])])
        v2 = RefutedCtx ["Failure2"] 
          (fromList [([mkName "Field2"], ["Field Failure 2"])])
        v3 = RefutedCtx ["Failure1", "Failure2"] 
          (fromList [([mkName "Field1"], ["Field Failure 1"]), ([mkName "Field2"], ["Field Failure 2"])])
      (v1 :: VCtx String [Int]) <! v2 `shouldBe` v3
