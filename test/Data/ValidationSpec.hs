{-# LANGUAGE
    FlexibleInstances
  , MultiParamTypeClasses
  , TemplateHaskellQuotes
  , TypeSynonymInstances
#-}

module Data.ValidationSpec where

------------------------------------------------------------------------------------------------------------------------
import Test.Hspec
import Data.Bool (bool)
import Data.Map (fromList)
import Text.Read (readEither)
------------------------------------------------------------------------------------------------------------------------
import Data.Validation
import Data.Validation.Internal
------------------------------------------------------------------------------------------------------------------------

spec :: Spec
spec = parallel $ do

  describe "Semigroup (Proof f a)" $ do
    it "If applied to two valid proofs, it concatenate the inhabitants." $ do
      Valid [1] <> Valid [2] `shouldBe` (Valid [1,2] :: Proof String [Int])

    it "If applied to an invalid and valid proof, it results in the invalid proof." $ do
      let
        vi = Invalid ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      (vi :: Proof String [Int]) <> Valid [2] `shouldBe` vi

    it "If applied to a valid and invalid proof, it results in the invalid proof." $ do
      let
        vi = Invalid ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      Valid [2] <> vi `shouldBe` (vi :: Proof String [Int])

    it "If applied to two invalid proofs, it concatenates the errors." $ do
      let
        vi1 = Invalid ["Failure 1"]
          (fromList [([mkName "Field1"], ["Field Failure 1"])])
        vi2 = Invalid ["Failure 2"]
          (fromList [([mkName "Field1"], ["Field Failure 1.1"]), ([mkName "Field2"], ["Field Failure 2"])])
        vi3 = Invalid ["Failure 1", "Failure 2"]
          (fromList [([mkName "Field1"], ["Field Failure 1", "Field Failure 1.1"]), ([mkName "Field2"], ["Field Failure 2"])])
      (vi1 :: Proof String [Int]) <> vi2 `shouldBe` vi3

  describe "Functor (Proof f)" $ do
    it "Converts a `Proof f a` to a `Proof f b`." $ do
      fmap show (Valid 1 :: Proof String Int) `shouldBe` Valid "1"

    it "Does not change the contents of an invalid proof." $ do
      let vi = Invalid ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])
      fmap show (vi :: Proof String Int) `shouldBe` (vi :: Proof String String)

  describe "Applicative (Proof f)" $ do
    let
      vap = Valid show :: Proof String (Int -> String)
      iap = Invalid ["Applicative Failure"] mempty
      vp = Valid 1 :: Proof String Int
      ip = Invalid ["Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])

    it "Constructs a new Valid proof" $ do
      pure 1 `shouldBe` (Valid 1 :: Proof String Int)

    it "If applied to two invalid proofs, it concatenates the errors." $ do
      (iap :: Proof String (Int -> String)) <*> ip
        `shouldBe` Invalid ["Applicative Failure", "Failure"] (fromList [([mkName "Field1"], ["Field Failure"])])

    it "If applied to an invalid and valid proof, it results in the invalid proof." $ do
      (iap :: Proof String (Int -> String)) <*> vp
        `shouldBe` (iap :: Proof String String)

    it "If applied to a valid and invalid proof, it results in the invalid proof." $ do
      vap <*> (ip :: Proof String Int) `shouldBe` (ip :: Proof String String)

    it "If applied to two valid proofs, it should transform the second proof." $ do
      vap <*> vp `shouldBe` Valid "1"

  describe "Monad (Proof f)" $ do
    it "Binds a valid proof to a function." $ do
      ((Valid 1 :: Proof String Int) >>= pure . show) `shouldBe` Valid "1"

    it "Does not change the contents of an invalid proof." $ do
      ((Invalid ["Failure"] mempty :: Proof String Int) >>= pure . show) `shouldBe` Invalid ["Failure"] mempty

  describe "fromVCtx" $ do
    let one = 1 :: Int

    it "Transforms a valid context to a valid proof." $ do
      fromVCtx (ValidCtx one :: VCtx String Int) `shouldBe` Valid one
    it "Transforms a disputed context to an invalid proof." $ do
      let
        v1 = DisputedCtx ["Failure"] (fromList [([mkName "Field1"],["Field Failure 1"])]) one
        v2 = Invalid ["Failure"] (fromList [([mkName "Field1"],["Field Failure 1"])])
      fromVCtx v1 `shouldBe` v2
    it "Transforms a refuted context to an invalid proof." $ do
      let
        v1 = RefutedCtx ["Failure"] (fromList [([mkName "Field1"],["Field Failure 1"])])
        v2 = Invalid ["Failure"] (fromList [([mkName "Field1"],["Field Failure 1"])])
      fromVCtx (v1 :: VCtx String Int) `shouldBe` v2

  describe "Functor ValueCtx" $ do
    it "transforms a global context." $ do
      fmap show (Global 1 :: ValueCtx Int) `shouldBe` Global "1"

    it "transforms a field context while preserving the field name." $ do
      fmap show (Field (mkName "Field1") 1 :: ValueCtx Int) `shouldBe` Field (mkName "Field1") "1"

  describe "getValue" $ do
    it "Retrieves the value from a global context." $ do
      getValue (Global "1") `shouldBe` "1"

    it "Retrieves the value from a field context." $ do
      getValue (Field (mkName "Field1") "1") `shouldBe` "1"

  describe "setValue" $ do
    it "Creates a global context based on the given context." $ do
      setValue (Global "1") (1 :: Int) `shouldBe` Global 1

    it "Retrieves the value from a field context." $ do
      setValue (Field (mkName "Field1") "1") (1 :: Int) `shouldBe` (Field (mkName "Field1") 1)

  describe "isRequired" $ do
    it "Adds a failure to the context if the value is Nothing." $ do
      let
        v = Global (Nothing :: Maybe Bool)
        r = isRequired "Failed" v
      r `shouldBe` refute v "Failed"

    it "Should add no failure if the value is Just." $ do
      let
        v = Global $ Just True
        v2 = Global True
        r = isRequired "Failed" v
      r `shouldBe` pure v2

  describe "isLeft" $ do
    it "Adds a failure to the context if the value is Right." $ do
      let
        v = Global (Right True :: Either Bool Bool)
        r = isLeft "Failed" v
      r `shouldBe` refute v "Failed"

    it "Should add no failure if the value is Left." $ do
      let
        v = Global $ (Left True :: Either Bool Bool)
        v2 = Global True
        r = isLeft "Failed" v
      r `shouldBe` pure v2

  describe "isRight" $ do
    it "Adds a failure to the context if the value is Left." $ do
      let
        v = Global (Left True :: Either Bool Bool)
        r = isRight "Failed" v
      r `shouldBe` refute v "Failed"

    it "Should add no failure if the value is Right." $ do
      let
        v = Global $ (Right True :: Either Bool Bool)
        v2 = Global True
        r = isRight "Failed" v
      r `shouldBe` pure v2

  describe "isNull" $ do
    it "Adds a failure to the context if the value is not null." $ do
      let
        v = Global "not null"
        r = isNull "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value is null." $ do
      let
        v = Global ""
        r = isNull "Failed" v
      r `shouldBe` pure v

  describe "isNotNull" $ do
    it "Adds a failure to the context if the value is null." $ do
      let
        v = Global ""
        r = isNotNull "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value is not null." $ do
      let
        v = Global "not null"
        r = isNotNull "Failed" v
      r `shouldBe` pure v

  describe "minLength" $ do
    it "Adds a failure to the context if the value has a length less than the given value." $ do
      let
        v = Global "TW"
        r = minLength 3 "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value has a length equal to the given value." $ do
      let
        v = Global "THR"
        r = minLength 3 "Failed" v
      r `shouldBe` pure v

    it "Should add no failure if the value has a length greater than the given value." $ do
      let
        v = Global "THREE"
        r = minLength 3 "Failed" v
      r `shouldBe` pure v

  describe "maxLength" $ do
    it "Adds a failure to the context if the value has a length greater than the given value." $ do
      let
        v = Global "FOUR"
        r = maxLength 3 "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value has a length equal to the given value." $ do
      let
        v = Global "THR"
        r = maxLength 3 "Failed" v
      r `shouldBe` pure v

    it "Should add no failure if the value has a length less than the given value." $ do
      let
        v = Global "TW"
        r = maxLength 3 "Failed" v
      r `shouldBe` pure v

  describe "isLength" $ do
    it "Adds a failure to the context if the value has a length greater than the given value." $ do
      let
        v = Global "FOUR"
        r = isLength 3 "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value has a length equal to the given value." $ do
      let
        v = Global "THR"
        r = isLength 3 "Failed" v
      r `shouldBe` pure v

    it "Adds a failure to the context if the value has a length less than the given value." $ do
      let
        v = Global "TW"
        r = isLength 3 "Failed" v
      r `shouldBe` dispute v "Failed"

  describe "isEqual" $ do
    it "Adds a failure to the context if the value is not equal to the given value." $ do
      let
        v = Global "1"
        r = isEqual "2" "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value is equal to the given value." $ do
      let
        v = Global "1"
        r = isEqual "1" "Failed" v
      r `shouldBe` pure v

  describe "isNotEqual" $ do
    it "Adds a failure to the context if the value is equal to the given value." $ do
      let
        v = Global "1"
        r = isNotEqual "1" "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value is not equal to the given value." $ do
      let
        v = Global "1"
        r = isNotEqual "2" "Failed" v
      r `shouldBe` pure v

  describe "isLessThan" $ do
    let
      one = 1 :: Int
      two = 2 :: Int
      three = 3 :: Int

    it "Adds a failure to the context if the value is greater than the given value." $ do
      let
        v = Global three
        r = isLessThan two "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Adds a failure to the context if the value is equal to the given value." $ do
      let
        v = Global two
        r = isLessThan two "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value is less than the given value." $ do
      let
        v = Global one
        r = isLessThan two "Failed" v
      r `shouldBe` pure v

  describe "isLessThanOrEqual" $ do
    let
      one = 1 :: Int
      two = 2 :: Int
      three = 3 :: Int

    it "Adds a failure to the context if the value is greater than the given value." $ do
      let
        v = Global three
        r = isLessThanOrEqual two "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value is equal to the given value." $ do
      let
        v = Global two
        r = isLessThanOrEqual two "Failed" v
      r `shouldBe` pure v

    it "Should add no failure if the value is less than the given value." $ do
      let
        v = Global one
        r = isLessThanOrEqual two "Failed" v
      r `shouldBe` pure v

  describe "isGreaterThan" $ do
    let
      one = 1 :: Int
      two = 2 :: Int
      three = 3 :: Int

    it "Should add no failure if the value is greater than the given value." $ do
      let
        v = Global three
        r = isGreaterThan two "Failed" v
      r `shouldBe` pure v

    it "Adds a failure to the context if the value is equal to the given value." $ do
      let
        v = Global two
        r = isGreaterThan two "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Adds a failure to the context if the value is less than the given value." $ do
      let
        v = Global one
        r = isGreaterThan two "Failed" v
      r `shouldBe` dispute v "Failed"

  describe "isGreaterThanOrEqual" $ do
    let
      one = 1 :: Int
      two = 2 :: Int
      three = 3 :: Int

    it "Should add no failure if the value is greater than the given value." $ do
      let
        v = Global three
        r = isGreaterThanOrEqual two "Failed" v
      r `shouldBe` pure v

    it "Should add no failure if the value is equal to the given value." $ do
      let
        v = Global two
        r = isGreaterThanOrEqual two "Failed" v
      r `shouldBe` pure v

    it "Adds a failure to the context if the value is less than the given value." $ do
      let
        v = Global one
        r = isGreaterThanOrEqual two "Failed" v
      r `shouldBe` dispute v "Failed"

  describe "hasElem" $ do
    it "Should add no failure if the value contains the given element." $ do
      let
        v = Global "test@example.com"
        r = hasElem '@' "Failed" v
      r `shouldBe` pure v

    it "Adds a failure to the context if the value does not contain the given element." $ do
      let
        v = Global "test.example.com"
        r = hasElem '@' "Failed" v
      r `shouldBe` dispute v "Failed"

  describe "doesNotHaveElem" $ do
    it "Adds a failure to the context if the value contains the given element." $ do
      let
        v = Global "test@example site.com"
        r = doesNotHaveElem ' ' "Failed" v
      r `shouldBe` dispute v "Failed"

    it "Should add no failure if the value does not contain the given element." $ do
      let
        v = Global "test@example-site.com"
        r = doesNotHaveElem ' ' "Failed" v
      r `shouldBe` pure v

  describe "ifAny" $ do
    it "Adds a failure to the context if no element in the list passes the check." $ do
      let
        v = Global ["foo", "bar", "baz", "bat"]
        r = ifAny (\e -> bool (Just (e ++ " does not equal buz")) Nothing $ e == "buz") v
      r `shouldBe` disputeMany v ["foo does not equal buz","bar does not equal buz","baz does not equal buz","bat does not equal buz"]

    it "Should add no failure if any element in the list passed the check." $ do
      let
        v = Global ["foo", "bar", "baz", "bat"]
        r = ifAny (\e -> bool (Just (e ++ " does not equal foo")) Nothing $ e == "foo") v
      r `shouldBe` pure v

  describe "ifAll" $ do
    it "Adds a failure to the context if any element in the list does not pass the check." $ do
      let
        v = Global ["foo", "bar", "baz", "bat"]
        r = ifAll (\e -> bool (Just (e ++ " does not have an 'a'")) Nothing $ elem 'a' e) v
      r `shouldBe` disputeMany v ["foo does not have an 'a'"]

    it "Should add no failure if every element in the list passed the check." $ do
      let
        v = Global ["bar", "baz", "bat"]
        r = ifAll (\e -> bool (Just (e ++ " does not have an 'a'")) Nothing $ elem 'a' e) v
      r `shouldBe` pure v

  describe "ifEach" $ do
    it "Adds a failure to the context if any element in the list does not pass the check." $ do
      let
        v = Global ["1", "2", "3", "bat"]
        r = ifEach (\e -> readEither e :: Either String Int) v
      r `shouldBe` refute v "Prelude.read: no parse"

    it "Should add no failure if every element in the list passed the check." $ do
      let
        v = Global ["1", "2", "3"]
        r = ifEach (\e -> readEither e :: Either String Int) v
        v2 = Global [1, 2, 3]
      r `shouldBe` pure v2

  describe "ifEachProven" $ do
    it "Adds a failure to the context if any element in the list does not pass the check." $ do
      let
        v = Global ["1234567", "7654321", "5555555", "bat"]
        r = ifEachProven mkPhoneNumber v
      r `shouldBe` refute v "Phone number is the wrong length."

    it "Should add no failure if every element in the list passed the check." $ do
      let
        v = Global ["1234567", "7654321", "5555555"]
        r = ifEachProven mkPhoneNumber v
        v2 = Global [PhoneNumber "1234567", PhoneNumber "7654321", PhoneNumber "5555555"]
      r `shouldBe` pure v2

  describe "isMatch" $ do
    it "Adds a failure to the context if value does not match the given value." $ do
      let
        v = Global "asdf"
        vm = pure "fdsa"
        r = isMatch "Failure" vm v
      r `shouldBe` dispute v "Failure"

    it "Should add no failure if the value matches the given value." $ do
      let
        v = Global "asdf"
        vm = pure "asdf"
        r = isMatch "Failure" vm v
      r `shouldBe` pure v

  describe "validateField" $ do
    it "Uses the `Validatable` instance on a subfield for validation." $ do
      let
        c = Field (mkName "contact") (ContactVM "")
      fromVCtx (validateField c) `shouldBe` Invalid [] (fromList [
          ([(mkName "contact"), 'phoneNumber], ["Phone Number cannot be empty."])
        ])

  describe "optional" $ do
    it "Nothing value is valid if optional" $ do
      let proof = fromVCtx $ optional (Nothing :: Maybe ContactVM) $ \v -> withValue v $ validateField
      proof `shouldBe` Valid Nothing
    it "Just value is will validate if optional" $ do
      let c = (ContactVM "")
      let proof = fromVCtx $ optional (Just c) $ \v -> withValue v $ validateField
      proof `shouldBe` Invalid [] (fromList [(['phoneNumber], ["Phone Number cannot be empty."])])

  describe "validateWhen" $ do
    it "Invalid value is Valid Nothing if validateWhen conditional is False" $ do
      let c = (ContactVM "")
      let proof = fromVCtx $ validateWhen False $ withValue c $ validateField
      proof `shouldBe` Valid Nothing
    it "Invalid value is invalid if validateWhen conditional is True" $ do
      let c = (ContactVM "")
      let proof = fromVCtx $ validateWhen True $ withValue c $ validateField
      proof `shouldBe` Invalid [] (fromList [(['phoneNumber], ["Phone Number cannot be empty."])])

data Contact
  = Contact
  { validPhoneNumber :: String
  } deriving (Show, Eq)

data ContactVM
  = ContactVM
  { phoneNumber :: String
  } deriving (Show, Eq)

newtype PhoneNumber = PhoneNumber { unPhoneNumber :: String }
  deriving (Show, Eq)
mkPhoneNumber :: String -> Proof String PhoneNumber
mkPhoneNumber s = fromVCtx $ do
  v <- withValue s (isLength 7 "Phone number is the wrong length.")
  return $ PhoneNumber v

instance Validatable String ContactVM Contact where
  validation c =
    let
      vp = withField 'phoneNumber (phoneNumber c) $ \v ->
        isNotNull "Phone Number cannot be empty." v
    in pure Contact <*> vp
