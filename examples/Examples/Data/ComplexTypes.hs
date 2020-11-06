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
  { userVMUsername          :: Maybe String
  , userVMEmailAddress      :: Maybe String
  , userVMPhoneNumber       :: Maybe String
  , userVMContactPreference :: Maybe ContactPreference
  , userVMZipCode           :: Maybe String
  } deriving (Show)

-- Actual Model
data User
  = User
  { userUsername          :: Username
  , userEmailAddress      :: Maybe EmailAddress
  , userPhoneNumber       :: Maybe PhoneNumber
  , userContactPreference :: ContactPreference
  , userZipCode           :: Maybe ZipCode
  } deriving (Show)

instance Validatable MyFailures UserVM User where
  validation u = do
    let vn = withField 'userVMUsername (userVMUsername u) $
          \n -> isRequired RequiredFailure n
          >>= refuteWithProof mkUsername
        -- EmailAdress and PhoneNumber are only required when ContactPreference matches
        ve = validateWhen (userVMContactPreference u == Just Email) $
          withField 'userVMEmailAddress (userVMEmailAddress u) $
            \e -> isRequired RequiredFailure e
            >>= refuteWithProof mkEmailAddress
        vp = validateWhen (userVMContactPreference u == Just Phone) $
          withField 'userVMPhoneNumber (userVMPhoneNumber u) $
            \p -> isRequired RequiredFailure p
            >>= refuteWithProof mkPhoneNumber
        cp = withField 'userVMContactPreference (userVMContactPreference u) $
          \p -> isRequired RequiredFailure p
        -- ZipCode is completely optional
        zc = optional (userVMZipCode u) $ \z ->
          withField 'userVMZipCode z $ refuteWithProof mkZipCode
        otherCheck = withValue u check
    pure User <*> vn <*> ve <*> vp <*> cp <*> zc <! otherCheck
    where
      -- arbitrary check
      check = disputeWithFact OtherFailure (\v -> userVMUsername v /= userVMEmailAddress v)
