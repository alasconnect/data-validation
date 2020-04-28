{-# LANGUAGE TypeFamilies #-}

module Data.Validation.Transforms where

-- | A type that represents a validated type.
data V
-- | A type that represents an unvalidated type, often called a View Model.
data VM

-- | A type that represents a validation transformaion.
-- The unvalidated type is the first parameter which is used when 'VM' is passed in.
-- The second parameter is the validated type which is used when 'V' is passed in.
--
-- ==== __Examples__
--
-- Basic usage:
--
-- @
-- data ThingV v 
--   = Thing 
--   { emailAddress :: VT v String EmailAddress
--   , confirmEmailAddress :: VT v String () 
--   }
-- type ThingVM = ThingV VM -- A `Thing` view model.
-- type Thing = ThingV V    -- A validated `Thing`.
-- @
type family VT v a b where
  VT V a b = b
  VT VM a b = a
