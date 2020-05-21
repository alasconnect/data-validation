module Data.Validation.Internal where

------------------------------------------------------------------------------------------------------------------------
import Prelude
import Data.Map
import Language.Haskell.TH (Name)
------------------------------------------------------------------------------------------------------------------------

-- | A type that holds aggregated validation failures.
data VCtx f a 
  = ValidCtx a -- ^ A value that is assumed to be valid.
  | DisputedCtx [f] (Map [Name] [f]) a -- ^ A value that has failures but can continue to be validated.
  | RefutedCtx [f] (Map [Name] [f]) -- ^ A value that has failures and cannot be validated further.
  deriving (Show, Eq)

instance Semigroup a => Semigroup (VCtx f a) where
  (ValidCtx a1)              <> (ValidCtx a2)              = ValidCtx (a1 <> a2)
  (ValidCtx a1)              <> (DisputedCtx gfs lfs a2)   = DisputedCtx gfs lfs (a1 <> a2)
  (ValidCtx _)               <> (RefutedCtx gfs lfs)       = RefutedCtx gfs lfs
  (DisputedCtx gfs lfs a1)   <> (ValidCtx a2)              = DisputedCtx gfs lfs (a1 <> a2)
  (DisputedCtx gfs1 lfs1 a1) <> (DisputedCtx gfs2 lfs2 a2) = 
    DisputedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2) (a1 <> a2)
  (DisputedCtx gfs1 lfs1 _)  <> (RefutedCtx gfs2 lfs2)     = RefutedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)
  (RefutedCtx gfs lfs)       <> (ValidCtx _)               = RefutedCtx gfs lfs
  (RefutedCtx gfs1 lfs1)     <> (DisputedCtx gfs2 lfs2 _)  = RefutedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)
  (RefutedCtx gfs1 lfs1)     <> (RefutedCtx gfs2 lfs2)     = RefutedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)

instance Functor (VCtx f) where
  fmap f (ValidCtx a)            = ValidCtx (f a)
  fmap f (DisputedCtx gps lfs a) = DisputedCtx gps lfs (f a)
  fmap _ (RefutedCtx gps lfs)    = RefutedCtx gps lfs

instance Applicative (VCtx f) where
  pure = ValidCtx
  (ValidCtx fn)              <*> (ValidCtx a)              = ValidCtx (fn a)
  (ValidCtx fn)              <*> (DisputedCtx gfs lfs a)   = DisputedCtx gfs lfs (fn a)
  (ValidCtx _)               <*> (RefutedCtx gfs lfs)      = RefutedCtx gfs lfs
  (DisputedCtx gfs lfs fn)   <*> (ValidCtx a)              = DisputedCtx gfs lfs (fn a)
  (DisputedCtx gfs1 lfs1 fn) <*> (DisputedCtx gfs2 lfs2 a) = 
    DisputedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2) (fn a)
  (DisputedCtx gfs1 lfs1 _)  <*> (RefutedCtx gfs2 lfs2)    = RefutedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)
  (RefutedCtx gfs lfs)       <*> (ValidCtx _)              = RefutedCtx gfs lfs
  (RefutedCtx gfs1 lfs1)     <*> (DisputedCtx gfs2 lfs2 _) = RefutedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)
  (RefutedCtx gfs1 lfs1)     <*> (RefutedCtx gfs2 lfs2)    = RefutedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)

instance Monad (VCtx f) where
  (ValidCtx a)            >>= fn = fn a
  (RefutedCtx gfs lfs)    >>= _  = RefutedCtx gfs lfs
  (DisputedCtx gfs lfs a) >>= fn = case fn a of
    ValidCtx b              -> DisputedCtx gfs lfs b
    DisputedCtx gfs' lfs' b -> DisputedCtx (gfs <> gfs') (unionWith (<>) lfs lfs') b
    RefutedCtx gfs' lfs'    -> RefutedCtx (gfs <> gfs') (unionWith (<>) lfs lfs')

-- | Takes the failures from the second parameter and adds them to the first.
aggregateFailures :: VCtx f a -> VCtx f b -> VCtx f a
aggregateFailures a b = a <! b

-- | Takes the failures from the right-hand-side, if any, and adds them to the left-hand-side.
(<!) :: VCtx f a -> VCtx f b -> VCtx f a
(ValidCtx a)              <! (ValidCtx _)              = ValidCtx a
(ValidCtx a)              <! (DisputedCtx gfs lfs _)   = DisputedCtx gfs lfs a
(ValidCtx _)              <! (RefutedCtx gfs lfs)      = RefutedCtx gfs lfs
(DisputedCtx gfs lfs a)   <! (ValidCtx _)              = DisputedCtx gfs lfs a
(DisputedCtx gfs1 lfs1 a) <! (DisputedCtx gfs2 lfs2 _) = DisputedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2) a
(DisputedCtx gfs1 lfs1 _) <! (RefutedCtx gfs2 lfs2)    = RefutedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)
(RefutedCtx gfs lfs)      <! (ValidCtx _)              = RefutedCtx gfs lfs
(RefutedCtx gfs1 lfs1)    <! (DisputedCtx gfs2 lfs2 _) = RefutedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)
(RefutedCtx gfs1 lfs1)    <! (RefutedCtx gfs2 lfs2)    = RefutedCtx (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)

testMatch :: Eq a => f -> a -> a -> Maybe f
testMatch f a1 a2 = case a1 == a2 of
  True  -> Nothing
  False -> Just f
