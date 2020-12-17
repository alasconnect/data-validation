module Examples.Data.ExampleSpec where

------------------------------------------------------------------------------------------------------------------------
import Prelude
import Data.Validation
import Test.Hspec
------------------------------------------------------------------------------------------------------------------------
import Examples.Data.ComplexTypes
import Examples.Data.Primitives
------------------------------------------------------------------------------------------------------------------------

spec :: Spec
spec = parallel $ do

  describe "Example Type Validation" $ do
    it "Valid Complex Type" $ do
      let validForm = UserVM (Just "Hugh Mann") (Just "person@earth.com") Nothing (Just Email) Nothing
          userProof = validate validForm
      userProof `shouldSatisfy` isValid
    it "Invalid Complex Type" $ do
      let invalidForm = UserVM (Just "broken") Nothing (Just "123") (Just Email) Nothing
          userFailure = validate invalidForm
      userFailure `shouldSatisfy` isInvalid
