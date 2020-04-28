{-# LANGUAGE 
    MultiParamTypeClasses
  , TemplateHaskellQuotes 
#-}

module Examples.Data.Primitives 
( MyFailure (..)
, Username
, mkUsername
, unUsername
, EmailAddress
, mkEmailAddress
, unEmailAddress
, PhoneNumber
, mkPhoneNumber
, unPhoneNumber
) where

------------------------------------------------------------------------------------------------------------------------
import Data.Validation
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
  v <- withValue s (matchesRegex "[a-zA-Z0-9+._-]+@[a-zA-Z-]+\\.[a-z]+" EmailFailure)
  return $ EmailAddress v

newtype PhoneNumber = PhoneNumber { unPhoneNumber :: String }
  deriving Show
mkPhoneNumber :: String -> Proof MyFailures PhoneNumber
mkPhoneNumber s = fromVCtx $ do
  v <- withValue s (isLength 7 LengthFailure)
  return $ PhoneNumber v
