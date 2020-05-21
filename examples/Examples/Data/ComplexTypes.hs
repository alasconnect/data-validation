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
  , userVMEmailAddress  :: Maybe String
  , userVMPhoneNumber   :: Maybe String
  } deriving (Show)

-- Actual Model
data User
  = User
  { userUsername     :: Username
  , userEmailAddress :: EmailAddress
  , userPhoneNumber  :: Maybe PhoneNumber
  } deriving (Show)

instance Validatable MyFailures UserVM User where
  validation u = do
    let vn = withField 'userVMUsername (userVMUsername u) $
          \n -> isRequired RequiredFailure n
          >>= refuteWithProof mkUsername
        ve = withField 'userVMEmailAddress (userVMEmailAddress u) $
          \e -> isRequired RequiredFailure e
          >>= refuteWithProof mkEmailAddress
        vp = optional (userVMPhoneNumber u) $ \un ->
          withField 'userVMPhoneNumber un $ refuteWithProof mkPhoneNumber
        otherCheck = withValue u check
    pure User <*> vn <*> ve <*> vp <! otherCheck
    where
      -- arbitrary check
      check = disputeWithFact OtherFailure (\v -> userVMUsername v /= userVMEmailAddress v)
