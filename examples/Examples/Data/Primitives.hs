{-# LANGUAGE
    MultiParamTypeClasses
  , TemplateHaskellQuotes
#-}

module Examples.Data.Primitives
( MyFailures (..)
, Username
, mkUsername
, unUsername
, EmailAddress
, mkEmailAddress
, unEmailAddress
, PhoneNumber
, mkPhoneNumber
, unPhoneNumber
, ContactPreference (..)
, PreferenceOther
, mkPreferenceOther
, unPreferenceOther
) where

------------------------------------------------------------------------------------------------------------------------
import Data.Validation
import Text.Regex.TDFA ((=~))
------------------------------------------------------------------------------------------------------------------------

data MyFailures
  = NameFailure
  | LengthFailure
  | EmailFailure
  | RequiredFailure
  | OtherFailure

instance Show MyFailures where
  show NameFailure = "that's a stupid name"
  show LengthFailure = "length wrong"
  show EmailFailure = "bad email"
  show RequiredFailure = "required field missing value"
  show OtherFailure = "something else wrong"

-- app specific types
newtype Username = Username { unUsername :: String }
  deriving Show
mkUsername :: String -> Proof MyFailures Username
mkUsername s = fromVCtx $ do
  v <- withValue s (isNotNull NameFailure)
  return $ Username v

newtype EmailAddress = EmailAddress { unEmailAddress :: String }
  deriving Show
mkEmailAddress :: String -> Proof MyFailures EmailAddress
mkEmailAddress s = fromVCtx $ do
  -- validates email matches a Regex using <https://hackage.haskell.org/package/regex-tdfa regex-tdfa>.
  v <- withValue s (disputeWithFact EmailFailure (flip (=~) "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+"))
  return $ EmailAddress v

newtype PhoneNumber = PhoneNumber { unPhoneNumber :: String }
  deriving Show
mkPhoneNumber :: String -> Proof MyFailures PhoneNumber
mkPhoneNumber s = fromVCtx $ do
  v <- withValue s (isLength 7 LengthFailure)
  return $ PhoneNumber v

data ContactPreference
  = Email | Phone | Other deriving (Enum, Eq, Show)

newtype PreferenceOther = PreferenceOther { unPreferenceOther :: String }
  deriving Show
mkPreferenceOther :: String -> Proof MyFailures PreferenceOther
mkPreferenceOther s = fromVCtx $ do
  v <- withValue s (isNotNull OtherFailure)
  return $ PreferenceOther v
