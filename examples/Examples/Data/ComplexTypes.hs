{-# LANGUAGE
    MultiParamTypeClasses
  , TemplateHaskellQuotes
#-}

module Examples.Data.ComplexTypes where

------------------------------------------------------------------------------------------------------------------------
import Data.Validation
------------------------------------------------------------------------------------------------------------------------
import Examples.Data.Primitives
------------------------------------------------------------------------------------------------------------------------

-- View Model
data UserVM
  = UserVM
  { userVMUsername      :: Maybe String
  , userVMHasEmail      :: Bool         -- Condition to store email
  , userVMEmailAddress  :: Maybe String
  , userVMPhoneNumber   :: Maybe String
  } deriving (Show)

-- Actual Model
data User
  = User
  { userUsername     :: Username
  , userEmailAddress :: Maybe EmailAddress -- Conditionally stored email
  , userPhoneNumber  :: Maybe PhoneNumber
  } deriving (Show)

instance Validatable MyFailures UserVM User where
  validation u = do
    let vn = withField 'userVMUsername (userVMUsername u) $
          \n -> isRequired RequiredFailure n
          >>= refuteWithProof mkUsername
        ve = requiredIf (userVMHasEmail u) RequiredFailure (userVMEmailAddress u) $
          \e -> withField 'userVMEmailAddress e $ refuteWithProof mkEmailAddress
        vp = optional (userVMPhoneNumber u) $ \un ->
          withField 'userVMPhoneNumber un $ refuteWithProof mkPhoneNumber
        otherCheck = withValue u check
    pure User <*> vn <*> ve <*> vp <! otherCheck
    where
      -- arbitrary check
      check = disputeWithFact OtherFailure (\v -> userVMUsername v /= userVMEmailAddress v)
