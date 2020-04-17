{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , TemplateHaskellQuotes
#-}

module Data.Validation
( VCtx -- do not export the constructors
, (<!)
, Proof(..)
, fromVCtx
, Validatable(..)
, ValueCtx(..)
, getValue
, setValue
, withField
, withValue
, refute
, dispute
-- Generic validations
, disputeWith
, disputeWithFact
, refuteWith
, refuteWithFact
, refuteWithProof
, isRequired
, minLength
, maxLength
, minValue
, maxValue
, isMatch
-- Validation Helpers
, validateField
, optional
-- Re-exports
, Name
, mkName
, nameBase
) where

------------------------------------------------------------------------------------------------------------------------
import Prelude hiding (foldl)
import Data.Bool
import Data.Char
import Data.List (intercalate)
import Data.Map hiding (null)
import Data.String
import Language.Haskell.TH (Name, mkName, nameBase)
------------------------------------------------------------------------------------------------------------------------

data VCtx e a = ValidCtx a | DisputedCtx [e] (Map [Name] [e]) a | RefutedCtx [e] (Map [Name] [e])

instance Semigroup (VCtx e a) where
  (ValidCtx _)              <> (ValidCtx a)              = ValidCtx a
  (ValidCtx _)              <> (DisputedCtx ges les a)   = DisputedCtx ges les a
  (ValidCtx _)              <> (RefutedCtx ges les)      = RefutedCtx ges les
  (DisputedCtx ges les _)   <> (ValidCtx a)              = DisputedCtx ges les a
  (DisputedCtx ges1 les1 _) <> (DisputedCtx ges2 les2 a) = DisputedCtx (ges1 <> ges2) (unionWith (<>) les1 les2) a
  (DisputedCtx ges1 les1 _) <> (RefutedCtx ges2 les2)    = RefutedCtx (ges1 <> ges2) (unionWith (<>) les1 les2)
  (RefutedCtx ges les)      <> (ValidCtx _)              = RefutedCtx ges les
  (RefutedCtx ges1 les1)    <> (DisputedCtx ges2 les2 _) = RefutedCtx (ges1 <> ges2) (unionWith (<>) les1 les2)
  (RefutedCtx ges1 les1)    <> (RefutedCtx ges2 les2)    = RefutedCtx (ges1 <> ges2) (unionWith (<>) les1 les2)

instance Functor (VCtx e) where
  fmap f (ValidCtx a)            = ValidCtx (f a)
  fmap f (DisputedCtx gps lps a) = DisputedCtx gps lps (f a)
  fmap _ (RefutedCtx gps lps)    = RefutedCtx gps lps

instance Applicative (VCtx e) where
  -- pure :: a -> VCtx e a
  pure = ValidCtx
  -- (<*>) :: VCtx e (a -> b) -> VCtx e a -> VCtx e b
  (ValidCtx f)              <*> (ValidCtx a)              = ValidCtx (f a)
  (ValidCtx f)              <*> (DisputedCtx ges les a)   = DisputedCtx ges les (f a)
  (ValidCtx _)              <*> (RefutedCtx ges les)      = RefutedCtx ges les
  (DisputedCtx ges les f)   <*> (ValidCtx a)              = DisputedCtx ges les (f a)
  (DisputedCtx ges1 les1 f) <*> (DisputedCtx ges2 les2 a) = DisputedCtx (ges1 <> ges2) (unionWith (<>) les1 les2) (f a)
  (DisputedCtx ges1 les1 _) <*> (RefutedCtx ges2 les2)    = RefutedCtx (ges1 <> ges2) (unionWith (<>) les1 les2)
  (RefutedCtx ges les)      <*> (ValidCtx _)              = RefutedCtx ges les
  (RefutedCtx ges1 les1)    <*> (DisputedCtx ges2 les2 _) = RefutedCtx (ges1 <> ges2) (unionWith (<>) les1 les2)
  (RefutedCtx ges1 les1)    <*> (RefutedCtx ges2 les2)    = RefutedCtx (ges1 <> ges2) (unionWith (<>) les1 les2)

instance Monad (VCtx e) where
  -- forall b c. VCtx e a -> (a -> VCtx e b) -> VCtx e b
  (ValidCtx a)            >>= f = f a
  (RefutedCtx ges les)    >>= _ = RefutedCtx ges les
  (DisputedCtx ges les a) >>= f = case f a of
    ValidCtx b              -> DisputedCtx ges les b
    DisputedCtx ges' les' b -> DisputedCtx (ges <> ges') (unionWith (<>) les les') b
    RefutedCtx ges' les'    -> RefutedCtx (ges <> ges') (unionWith (<>) les les')

(<!) :: VCtx e a -> VCtx e b -> VCtx e a
(ValidCtx a)              <! (ValidCtx _)              = ValidCtx a
(ValidCtx a)              <! (DisputedCtx ges les _)   = DisputedCtx ges les a
(ValidCtx a)              <! (RefutedCtx ges les)      = RefutedCtx ges les
(DisputedCtx ges les a)   <! (ValidCtx _)              = DisputedCtx ges les a
(DisputedCtx ges1 les1 a) <! (DisputedCtx ges2 les2 _) = DisputedCtx (ges1 <> ges2) (unionWith (<>) les1 les2) a
(DisputedCtx ges1 les1 _) <! (RefutedCtx ges2 les2)    = RefutedCtx (ges1 <> ges2) (unionWith (<>) les1 les2)
(RefutedCtx ges les)      <! (ValidCtx _)              = RefutedCtx ges les
(RefutedCtx ges1 les1)    <! (DisputedCtx ges2 les2 _) = RefutedCtx (ges1 <> ges2) (unionWith (<>) les1 les2)
(RefutedCtx ges1 les1)    <! (RefutedCtx ges2 les2)    = RefutedCtx (ges1 <> ges2) (unionWith (<>) les1 les2)

data Proof e a = Valid a | Invalid [e] (Map [Name] [e]) deriving (Show)

instance Semigroup (Proof e a) where
  (Valid _)           <> (Valid a)            = Valid a
  (Invalid ges1 les1) <> (Invalid ges2 les2)  = Invalid (ges1 <> ges2) (unionWith (<>) les1 les2)
  (Valid _)           <> (Invalid ges les)    = Invalid ges les
  (Invalid ges les)   <> (Valid _)            = Invalid ges les

instance Functor (Proof e) where
  fmap f (Valid a)         = Valid (f a)
  fmap _ (Invalid gps lps) = Invalid gps lps

instance Applicative (Proof e) where
  -- pure :: a -> Proof e a
  pure = Valid
  -- (<*>) :: Proof e (a -> b) -> Proof e a -> Proof e b
  (Invalid ges1 les1) <*> (Invalid ges2 les2) = Invalid (ges1 <> ges2) (unionWith (<>) les1 les2)
  (Invalid ges les)   <*> (Valid _)           = Invalid ges les
  (Valid _)           <*> (Invalid ges les)   = Invalid ges les
  Valid f             <*> Valid a             = Valid $ f a

instance Monad (Proof e) where
  -- forall b c. Proof e a -> (a -> Proof e b) -> Proof e b
  (Invalid ges les) >>= _ = Invalid ges les
  (Valid a)         >>= f = f a

fromVCtx :: VCtx e a -> Proof e a
fromVCtx (ValidCtx a)            = Valid a
fromVCtx (DisputedCtx ges les _) = Invalid ges les
fromVCtx (RefutedCtx ges les)    = Invalid ges les

class Validatable e a b | a -> e b where
  validation :: a -> VCtx e b

validate :: Validatable e a b => a -> Proof e b
validate = fromVCtx . validation

data ValueCtx a
  = Field Name a
  | Global a

getValue :: ValueCtx a -> a
getValue (Field _ a) = a
getValue (Global a)  = a

setValue :: ValueCtx a -> b -> ValueCtx b
setValue (Field n _) b = Field n b
setValue (Global _) b  = Global b

withField :: Name -> a -> (ValueCtx a -> VCtx e (ValueCtx b)) -> VCtx e b
withField n a f = f (Field n a) >>= pure . getValue

withValue :: a -> (ValueCtx a -> VCtx e (ValueCtx b)) -> VCtx e b
withValue a f = f (Global a) >>= pure . getValue

refute :: ValueCtx a -> e -> VCtx e b
refute (Field n _) e = RefutedCtx [] (singleton [n] [e])
refute (Global _) e  = RefutedCtx [e] empty

dispute :: ValueCtx a -> e -> VCtx e (ValueCtx a)
dispute v@(Field n _) e = DisputedCtx [] (singleton [n] [e]) v
dispute v@(Global _) e  = DisputedCtx [e] empty v

-- Standard validation functions can be created to make it easier to perform common validations.
disputeWith :: (a -> Maybe e) -> ValueCtx a -> VCtx e (ValueCtx a)
disputeWith f v = case f (getValue v) of
  Just e  -> dispute v e
  Nothing -> pure v

disputeWithFact :: e -> (a -> Bool) -> ValueCtx a -> VCtx e (ValueCtx a)
disputeWithFact e f = disputeWith (bool (Just e) Nothing . f)

refuteWith :: (a -> Either e b) -> ValueCtx a -> VCtx e (ValueCtx b)
refuteWith f v = case f (getValue v) of
  Left e  -> refute v e
  Right b -> pure $ setValue v b

refuteWithFact :: e -> (a -> Bool) -> ValueCtx a -> VCtx e (ValueCtx a)
refuteWithFact e f = refuteWith (\a -> bool (Left e) (Right a) $ f a)

refuteWithProof :: (a -> Proof e b) -> ValueCtx a -> VCtx e (ValueCtx b)
refuteWithProof f (Global a) = case f a of
  Invalid ges les -> RefutedCtx ges les
  Valid b         -> ValidCtx $ Global b
refuteWithProof f (Field n a) = case f a of
  Invalid ges les  -> RefutedCtx [] $ insert [n] ges les
  Valid b          -> ValidCtx $ Field n b

isRequired :: e -> ValueCtx (Maybe a) -> VCtx e (ValueCtx a)
isRequired e = refuteWith $ \ma -> case ma of
  Nothing -> Left e
  Just a  -> Right a

minLength :: (IsString a, Show a) => Int -> e -> ValueCtx a -> VCtx e (ValueCtx a)
minLength l e = disputeWith $ bool (Just e) Nothing . (<=) l . length . show

maxLength :: (IsString a, Show a) => Int -> e -> ValueCtx a -> VCtx e (ValueCtx a)
maxLength l e = disputeWith $ bool (Just e) Nothing . (>=) l . length . show

minValue :: (Num a, Ord a) => a -> e -> ValueCtx a -> VCtx e (ValueCtx a)
minValue l e = disputeWith $ bool (Just e) Nothing . (<=) l

maxValue :: (Num a, Ord a) => a -> e -> ValueCtx a -> VCtx e (ValueCtx a)
maxValue l e = disputeWith $ bool (Just e) Nothing . (>=) l

isMatch :: Eq a => e -> VCtx e a -> ValueCtx a -> VCtx e (ValueCtx a)
isMatch e (ValidCtx a)        = disputeWith (testMatch e a)
isMatch e (DisputedCtx _ _ a) = disputeWith (testMatch e a)
isMatch e (RefutedCtx _ _)    = pure

testMatch :: Eq a => e -> a -> a -> Maybe e
testMatch e a1 a2 = case a1 == a2 of
  True  -> Nothing
  False -> Just e

validateField :: Validatable e a b => Name -> a -> VCtx e b
validateField n a = case validation a of
  ValidCtx b            -> ValidCtx b
  DisputedCtx ges les b -> DisputedCtx [] (insert [n] ges $ mapKeys (\k -> [n] ++ k) les) b
  RefutedCtx ges les    -> RefutedCtx [] (insert [n] ges $ mapKeys (\k -> [n] ++ k) les)

optional :: Maybe a -> (a -> VCtx e b) -> VCtx e (Maybe b)
optional Nothing _  = ValidCtx Nothing
optional (Just a) f =
  case f a of
    ValidCtx b            -> ValidCtx (Just b)
    DisputedCtx ges les b -> DisputedCtx ges les (Just b)
    RefutedCtx ges les    -> RefutedCtx ges les
