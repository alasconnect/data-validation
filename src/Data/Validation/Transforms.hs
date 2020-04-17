{-# LANGUAGE TypeFamilies #-}

module Data.Validation.Transforms where

data V
data UV

type family VT v a b where
  VT V a b = b
  VT UV (Maybe a) b = Maybe a
  VT UV a b = Maybe a
