{-# LANGUAGE
    MultiParamTypeClasses
  , FlexibleContexts
  , FunctionalDependencies
  , TemplateHaskellQuotes
#-}

module Data.Validation
( -- * Basics
-- $basics

-- ** Proof
-- $proof
  Proof(..)
, fromVCtx
-- ** ValueCtx
-- $valuectx
, ValueCtx(..)
, getValue
, setValue
, withField
, withValue
-- * Validating Primitives
-- $primitives

-- * Validating Complex Types
-- $complex_types
, Validatable(..)
, validate
-- * Dispute and Refute
-- $dispute_vs_refute
, refute
, refuteMany
, refuteWith
, refuteWithProof
, dispute
, disputeMany
, disputeWith
, disputeWithFact
-- * General Validators
, isRequired
, isLeft
, isRight
, isNull
, isNotNull
, minLength
, maxLength
, isLength
, isEqual
, isNotEqual
, isLessThan
, isLessThanOrEqual
, isGreaterThan
, isGreaterThanOrEqual
, hasElem
, doesNotHaveElem
, ifAny
, ifAll
, ifEach
, ifEachProven
, isMatch
-- * Validation Helpers
, validateField
, optional
, aggregateFailures
, (<!)
, isValid
, isInvalid
, flattenProofs
-- * Re-exports
, VCtx
, Name
, mkName
, nameBase
) where

------------------------------------------------------------------------------------------------------------------------
import Prelude hiding (foldl)
import Data.Bool
import Data.Either (Either(..), rights, lefts)
import Data.Foldable (fold)
import Data.Map hiding (null, fold)
import Data.Maybe
import Language.Haskell.TH (Name, mkName, nameBase)
------------------------------------------------------------------------------------------------------------------------
import Data.Validation.Internal
------------------------------------------------------------------------------------------------------------------------

{- $basics
  Validation generally takes the form of @a -> Either f b@ where:

  [@a@]: Some unvalidated type.

  [@b@]: Some validated type.

  [@f@]: Some failure type.

  Consider the following example:

  @
  data MyFailures = EmptyEmailAddress | MalformedEmailAddress
  validateEmailAddress :: String -> Either MyFailures EmailAddress
  @

  In this case:

  * @a@ ~ 'String'
  * @b@ ~ EmailAddress
  * @f@ ~ MyFailures

-}

{- $proof

  The transformation from @a@ to @b@ is important and provides a type safe way to prove that validation was successful.
  However, rather than using the 'Either' type, this library uses the 'Proof' type.
  A 'Proof' represents either a validated type or a collection of failures.
  Notice, we use the term validation /failures/ instead of /errors/
  to differentiate between validation and error handling.
  The reason we use the 'Proof' type is because it has a custom 'Control.Applicative.Applicative' instance
  that will be helpful later.

  The 'Invalid' constructor takes a list of global failures and a map of field failures.
  Field failures are useful for identifying a specific field in a record that is invalid.
  Fields are identified using a list of 'Language.Haskell.TH.Name' types.
  There are two ways to create this type: the @TemplateHaskellQuotes@ extension and the 'Language.Haskell.TH.mkName' function.

  Using the @TemplateHaskellQuotes@ language extension, you can easily create 'Language.Haskell.TH.Name's using a special syntax.
  For a records like:

  > data User = User { emailAddress :: String }

  The name can be retrieved by referencing the name with a single quote in front:

  > let name = 'emailAddress

  This allows for the consistant and type safe generation of names.
  This method does generate a fully qualified name that includes the module name.
  The base name can be accessed using the 'Language.Haskell.TH.nameBase' function.
  This extension is considered safe Haskell while the @TemplateHaskell@ extension is not.
  See <https://ghc.gitlab.haskell.org/ghc/doc/users_guide/exts/template_haskell.html#syntax> for details.

  Another approach is to use the 'Language.Haskell.TH.mkName' function.
  This allows field names to be generated for non-record types.
  However, it does require managing magic strings.

  To finish our discussion on the 'Proof' type, the reason the key is a list of names is because of subfields.
  Consider a record like this:

  @
  data Contact = Contact { phoneNumber :: String }

  data User
    = User
    { username :: String
    , contact :: Contact
    }
  @

  In this case, validating the `User` type requires validating the `contact` field which is a `Contact`.
  In that case, the key would need to identify that the `phoneNumber` field is a subfield of `contact`.
  The key would look like this:

  > ['contact, 'phoneNumber]
-}

-- | A type that holds either validation failures or a validated value.
data Proof f a
  = Valid a -- ^ A validated value.
  | Invalid [f] (Map [Name] [f]) -- ^ Global and field validation failures.
  deriving (Show, Eq)

instance Semigroup a => Semigroup (Proof f a) where
  (Valid a1)          <> (Valid a2)           = Valid (a1 <> a2)
  (Invalid gfs1 lfs1) <> (Invalid gfs2 lfs2)  = Invalid (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)
  (Valid _)           <> (Invalid gfs lfs)    = Invalid gfs lfs
  (Invalid gfs lfs)   <> (Valid _)            = Invalid gfs lfs

instance Monoid a => Monoid (Proof f a) where
  mempty = Valid mempty

instance Functor (Proof f) where
  fmap fn (Valid a)        = Valid (fn a)
  fmap _ (Invalid gps lps) = Invalid gps lps

instance Applicative (Proof f) where
  pure = Valid
  (Invalid gfs1 lfs1) <*> (Invalid gfs2 lfs2) = Invalid (gfs1 <> gfs2) (unionWith (<>) lfs1 lfs2)
  (Invalid gfs lfs)   <*> (Valid _)           = Invalid gfs lfs
  (Valid _)           <*> (Invalid gfs lfs)   = Invalid gfs lfs
  Valid fn            <*> Valid a             = Valid $ fn a

instance Monad (Proof f) where
  (Invalid gfs lfs) >>= _  = Invalid gfs lfs
  (Valid a)         >>= fn = fn a

{- | Converts a 'Data.Validation.Internal.VCtx' to a 'Proof'.

  Internally, this library uses the 'Data.Validation.Internal.VCtx' type to track validation failures.
  This is because a validation failure can be partial.
  For instance, checking that a password has a special character can happen even if the check for a numeric character
  has already failed.
  This allows validation to discover as many failures as possible.

  However, once validation is complete, the result becomes binary.
  The validation has either succeeded or failed.
  In order to convert from a 'Data.Validation.Internal.VCtx' to a 'Proof', use the 'fromVCtx' function.
-}
fromVCtx :: VCtx f a -> Proof f a
fromVCtx (ValidCtx a)            = Valid a
fromVCtx (DisputedCtx gfs lfs _) = Invalid gfs lfs
fromVCtx (RefutedCtx gfs lfs)    = Invalid gfs lfs

{- $valuectx
  This library uses composable validations.
  As such, we need a type that carries information about the thing being validated.
  Specifically, we need a type to carry its value and, optionally, its name.

  A value with a name is a field and will be used to create field failures.
  A value without a name will be used to create global failures.
  This is accomplished using the 'ValueCtx' type.

  The 'withField' and 'withValue' functions are used to create these types and perform validation at the same time.
  These functions save the effort of wrapping and unwrapping values with the 'ValueCtx' type.
-}

-- | A type for storing a value to validate and, optionally, its field name.
data ValueCtx a
  = Field Name a -- ^ The 'Field' constructor represents a value that is named.
  | Global a     -- ^ The 'Global' constructor represents a value that is not named.
  deriving (Show, Eq)

instance Functor ValueCtx where
  fmap fn (Field n a) = Field n (fn a)
  fmap fn (Global a)  = Global (fn a)

-- | Accessor for a 'ValueCtx''s value.
getValue :: ValueCtx a -> a
getValue (Field _ a) = a
getValue (Global a)  = a

-- | Replaces the existing value with a new one without changing the name, if one exists.
setValue :: ValueCtx a -> b -> ValueCtx b
setValue (Field n _) b = Field n b
setValue (Global _) b  = Global b

-- | Performs some given validation using a 'Field' with a given name and value.
withField :: Name -> a -> (ValueCtx a -> VCtx f (ValueCtx b)) -> VCtx f b
withField n a fn = fn (Field n a) >>= pure . getValue

-- | Performs some given validation using a 'Global' with a given value.
withValue :: a -> (ValueCtx a -> VCtx f (ValueCtx b)) -> VCtx f b
withValue a fn = fn (Global a) >>= pure . getValue

{- $primitives
  Validating primitives is a special case because the validated type and unvalidated type are often the same.
  An email address is just a specially formatted string, so both the unvalidated email address and the validated one have the same types.
  To get around this, you can wrap a primitive in a `newtype`, hide the constructor, and create a smart constructor that validates the input.
  This allows for a kind of type safety known as `correct by construction`.
  Consider the following example.

  @
  module Primitives
  ( MyFailures
  , EmailAddress -- the constructors are not exported
  , mkEmailAddress
  ) where

  data MyFailures = EmptyEmail |

  mkEmailAddress :: String -> Proof MyFailures EmailAddress
  mkEmailAddress s = fromVCtx $ do              -- (1)
    v \<- withValue s $ \\v -> do                 -- (2)
      isNotNull EmptyEmail v                    -- (3)
      disputeWithFact InvalidEmail (elem '@') v -- (4)
    return $ EmailAddress v                     -- (5)
  @

  Starting with line (2), the 'withValue' function is used to create a 'ValueCtx' using string passed into the function.
  The lambda function that follows takes the 'ValueCtx' and runs it through several validators using `do` syntax.
  This is possible because 'VCtx' has a 'Monad' instance.

  Line (1) transforms the result from a 'VCtx' to a 'Proof' as discussed above.

  Line (3) and (4) demonstrate both a general validator and a custom validator.
  These will be covered in more detail later.

  Line (5) constructs the `EmailAddress` type and returns it.
  If any validation failures occur before this point, an 'Invalid' result is generated instead of the `EmailAddress`.

  It is important to note that no instances should be created for the `EmailAddress` type that allow construction.
  This includes the `FromJSON` type class from the `aeson` library.
-}

{- $complex_types
  In general, validating complex types works the same way as validating primitives.
  It starts with an unvalidated type that is transformed into a validated type.
  These unvalidated types are often called "view models".
  View models should be transformed into models when they are validated.
  So, like primitives, validating complex types should have the form validate :: a -> Proof f b.

  For complex types, this can be accomplished with the 'Validatable f a b' type class.
  This type class takes 3 parameters: the failure type, the view model type, and the final model type.
  It requires the implementation of a single function: 'validation'.
  Then the 'validate' function can be used to perform the actual validation.
  Consider the following example:

  @
  -- unvalidated type
  data UserCreatableVM
    = UserCreatableVM
    { userCreatableVMEmailAddress         :: String
    , userCreatableVMConfirmEmailAddress  :: String
    , userCreatableVMPassword             :: String
    , userCreatableVMConfirmPassword      :: String
    , userCreatableVMName                 :: Maybe String
    }

  -- validated type
  data UserCreatable
    = UserCreatable                                                                                     -- (1)
    { userCreatableEmailAddress :: EmailAddress
    , userCreatablePassword     :: Password
    , userCreatableName         :: Maybe Name
    }

  instance Validatable MyFailureType UserCreatableVM UserCreatable where                                -- (2)
    validation u =
      let ve = withField 'userCreatableVMEmailAddress (userCreatableVMEmailAddress u) $
            refuteWithProof mkEmailAddress                                                              -- (3)
          vce = withField 'userCreatableVMConfirmEmailAddress (userCreatableVMConfirmEmailAddress u) $
            \\ce -> refuteWithProof mkEmailAddress ce
            >>= isMatch MismatchedEmail ve                                                              -- (4)
          vp = withField 'userCreatableVMPassword (userCreatableVMPassword u) $
            refuteWithProof mkPassword
          vcp = withField 'userCreatableVMConfirmPassword (userCreatableVMConfirmPassword u) $
            \\ce -> refuteWithProof mkPassword ce
            >>= isMatch MismatchedPassword vp
          vn = optional (userCreatableVMName u) $ \\n ->
              withField 'userCreatableVMName n $ refuteWithProof mkName
          otherCheck = withValue u nameNotInPassword
      in pure UserCreatable \<*> ve \<*> vp \<*> vn <! vce <! vcp <! otherCheck                            -- (5)
    where nameNotInPassword = ...
  @

  The final model does not have all of the same fields on line (1).
  The confirmation fields were removed because they serve no purpose beyond validation.

  The validation function is implemented much like the smart constructors in the previous section.
  However, it is using the 'withField' function instead of 'withValue'.
  In addition, there is no call to 'fromVCtx' because validation is expected to return a 'VCtx'.
  Another function, 'validate', will use these validations to produce a 'Proof'.

  On line (2), the 'Validatable' instance is declared with an application specific failure type.
  The second and third parameter are the view model and final model types.
  This represents the transformation from the unvalidated type to the validated types.

  On line (3), there is a call to 'refuteWithProof' which validates and constructs a primitive using the smart constructor from the previous example.
  Line (4) uses the 'isMatch' function to prove that the email address and confirm email address fields match.
  The function accepts a 'VCtx' to match against making it very easy to compare validated field.

  Finally, line (5) is a bit interesting.
  It constructs the final type using applicative syntax.
  It uses the applicative instance on 'VCtx' to construct the final type.
  If all of the parameters are valid, the expression returns a valid `UserCreatable`.
  However, if any of the parameters are invalid, the whole expression becomes invalid and contains every failure from every field.
  This creates the aggregated result.

  There is also a call to the '(<!)' function.
  This function is read as 'aggregateFailures'.
  In English, it takes the failures from the second parameter, if any, and adds them to the first.
  This allows the aggregation of failures from fields that are not included in the final type.
-}

-- | A type class that represents a value that can be validated.
--
-- The parameters represent the following:
--
-- * @f@: the type of validation failures.
--
-- * @a@: the unvalidated type or view model.
--
-- * @b@: the validated type.
class Validatable f a b | a -> f b where
  validation :: a -> VCtx f b

-- | Runs the validations for a given value and returns the proof.
validate :: Validatable f a b => a -> Proof f b
validate = fromVCtx . validation

{- $dispute_vs_refute
  Refuting a value stops all validation efforts on the value.
  This means that any future failures that could have been detected will not.
  Dispute, on the other hand, will allow validation to continue.
  So, why should one be chosen over the other?

  First, we have to look at how validation works.
  Validation transforms values from an unvalidated type to a validated type.
  So, when a value is being passed through a validation chain, it is being transformed.
  If a validation fails, the transform fails too; there is no way around this.
  The new value cannot be retrieved from the validation if it failed.

  Consider a @'Maybe' 'String'@ value that is required and must be at least 3 characters long.
  First, the value would pass through the 'isRequired' validator.
  If the value is a @'Just' a@, validation succeeds and the value a is passed to the next validator which checks its length.
  If the value is a 'Nothing', it is not possible to check its length.
  Therefore, the validation must be refuted.
  A refuted value results in an invalid 'Proof' but stops the execution of any further validation.

  Now, consider an 'Int' value that must be greater than 2 and even.
  First, the value would pass though the 'minValue' validator.
  If the value is greater than 2, validation succeeds and the `isEven` validator is called.
  If the value is 2 or less, the validation fails.
  However, rather than fail completely, the next validator can just use the same value that was passed into the 'minValue' validator.
  In that case, the validator should dispute the value so that the next validator can be run.
  This will still result in an invalid 'Proof' but allows for more failures to be detected.

  In general, if a validator has the form @a -> 'Either' f b@, a failure must be refuted because they transform the value.
  If it has the form @a -> 'Maybe' f@, it should be disputed because it does not transform the value.
-}

-- | Adds a validation failure to the result and ends validation.
refute :: ValueCtx a -> f -> VCtx f b
refute (Field n _) f = RefutedCtx [] (singleton [n] [f])
refute (Global _) f  = RefutedCtx [f] empty

-- | Adds validation failures to the result and ends validation.
refuteMany :: ValueCtx a -> [f] -> VCtx f b
refuteMany (Field n _) fs = RefutedCtx [] (singleton [n] fs)
refuteMany (Global _) fs  = RefutedCtx fs empty

-- | Adds a validation failure to the result and continues validation.
dispute :: ValueCtx a -> f -> VCtx f (ValueCtx a)
dispute v@(Field n _) f = DisputedCtx [] (singleton [n] [f]) v
dispute v@(Global _) f  = DisputedCtx [f] empty v

-- | Adds validation failures to the result and continues validation.
disputeMany :: ValueCtx a -> [f] -> VCtx f (ValueCtx a)
disputeMany v@(Field n _) fs = DisputedCtx [] (singleton [n] fs) v
disputeMany v@(Global _) fs  = DisputedCtx fs empty v

-- | Performs a validation using a given function and handles the result.
-- If the result is `Just f`, a validation failure is added to the result and validation continues.
-- If the result is `Nothing`, validation continues with no failure.
disputeWith :: (a -> Maybe f) -> ValueCtx a -> VCtx f (ValueCtx a)
disputeWith fn v = case fn (getValue v) of
  Just f  -> dispute v f
  Nothing -> pure v

-- | Similar to 'disputeWith' except that the given failure is added if the given function returns False.
disputeWithFact :: f -> (a -> Bool) -> ValueCtx a -> VCtx f (ValueCtx a)
disputeWithFact f fn = disputeWith (bool (Just f) Nothing . fn)

-- | Performs a validation using a given function and handles the result.
-- If the result is `Left f`, a validation failure is added to the result and validation ends.
-- If the result is `Right b`, validation continues with the new value.
refuteWith :: (a -> Either f b) -> ValueCtx a -> VCtx f (ValueCtx b)
refuteWith fn v = case fn (getValue v) of
  Left f  -> refute v f
  Right b -> pure $ setValue v b

-- | Performs a validation using a given function and handles the result.
-- If the result is 'Invalid', the validation failures are added to the result and validation ends.
-- If the result is `Valid b`, validation continues with the new value.
refuteWithProof :: (a -> Proof f b) -> ValueCtx a -> VCtx f (ValueCtx b)
refuteWithProof f (Global a) = case f a of
  Invalid gfs lfs -> RefutedCtx gfs lfs
  Valid b         -> ValidCtx $ Global b
refuteWithProof f (Field n a) = case f a of
  Invalid gfs lfs  -> RefutedCtx [] $ insert [n] gfs lfs
  Valid b          -> ValidCtx $ Field n b

-- General Validators

-- | Checks that a 'Data.Maybe.Maybe' value is a 'Data.Maybe.Just'.
-- If not, it adds the given failure to the result and validation end.
isRequired :: f -> ValueCtx (Maybe a) -> VCtx f (ValueCtx a)
isRequired f = refuteWith $ \ma -> case ma of
  Nothing -> Left f
  Just a  -> Right a

-- | Checks that a 'Data.Either.Either' value is a 'Data.Either.Left'.
-- If not, it adds the given failure to the result and validation end.
isLeft :: f -> ValueCtx (Either a b) -> VCtx f (ValueCtx a)
isLeft f = refuteWith $ \e -> case e of
  Left a  -> Right a
  Right _ -> Left f

-- | Checks that a 'Data.Either.Either' value is a 'Data.Either.Right'.
-- If not, it adds the given failure to the result and validation end.
isRight :: f -> ValueCtx (Either a b) -> VCtx f (ValueCtx b)
isRight f = refuteWith $ \e -> case e of
  Right b -> Right b
  Left _  -> Left f

-- | Checks that the 'Foldable' is empty.
-- If not, it adds the given failure to the result and validation continues.
isNull :: Foldable t => f -> ValueCtx (t a) -> VCtx f (ValueCtx (t a))
isNull f = disputeWith $ bool (Just f) Nothing . null

-- | Checks that the 'Foldable' is not empty.
-- If empty, it adds the given failure to the result and validation continues.
isNotNull :: Foldable t => f -> ValueCtx (t a) -> VCtx f (ValueCtx (t a))
isNotNull f = disputeWith $ bool (Just f) Nothing . not . null

-- | Checks that a 'Foldable' has a length equal to or greater than the given value.
-- If not, it adds the given failure to the result and validation continues.
minLength :: Foldable t => Int -> f -> ValueCtx (t a) -> VCtx f (ValueCtx (t a))
minLength l f = disputeWith $ bool (Just f) Nothing . (<=) l . length

-- | Checks that a 'Foldable' has a length equal to or less than the given value.
-- If not, it adds the given failure to the result and validation continues.
maxLength :: Foldable t => Int -> f -> ValueCtx (t a) -> VCtx f (ValueCtx (t a))
maxLength l f = disputeWith $ bool (Just f) Nothing . (>=) l . length

-- | Checks that a 'Foldable' has a length equal to the given value.
-- If not, it adds the given failure to the result and validation continues.
isLength :: Foldable t => Int -> f -> ValueCtx (t a) -> VCtx f (ValueCtx (t a))
isLength l f = disputeWith $ bool (Just f) Nothing . (==) l . length

-- | Checks that a value is equal to another.
-- If not, it adds the given failure to the result and validation continues.
isEqual :: Eq a => a -> f -> ValueCtx a -> VCtx f (ValueCtx a)
isEqual a f = disputeWith $ bool (Just f) Nothing . (==) a

-- | Checks that a value is not equal to another.
-- If equal, it adds the given failure to the result and validation continues.
isNotEqual :: Eq a => a -> f -> ValueCtx a -> VCtx f (ValueCtx a)
isNotEqual a f = disputeWith $ bool (Just f) Nothing . (/=) a

-- | Checks that a value is less than another.
-- If not, it adds the given failure to the result and validation continues.
isLessThan :: Ord a => a -> f -> ValueCtx a -> VCtx f (ValueCtx a)
isLessThan a f = disputeWith $ bool (Just f) Nothing . (>) a

-- | Checks that a value is greater than another.
-- If not, it adds the given failure to the result and validation continues.
isGreaterThan :: Ord a => a -> f -> ValueCtx a -> VCtx f (ValueCtx a)
isGreaterThan a f = disputeWith $ bool (Just f) Nothing . (<) a

-- | Checks that a value is less than or equal to another.
-- If not, it adds the given failure to the result and validation continues.
isLessThanOrEqual :: Ord a => a -> f -> ValueCtx a -> VCtx f (ValueCtx a)
isLessThanOrEqual a f = disputeWith $ bool (Just f) Nothing . (>=) a

-- | Checks that a value is greater than or equal to another.
-- If not, it adds the given failure to the result and validation continues.
isGreaterThanOrEqual :: Ord a => a -> f -> ValueCtx a -> VCtx f (ValueCtx a)
isGreaterThanOrEqual a f = disputeWith $ bool (Just f) Nothing . (<=) a

-- | Checks that a 'Foldable' has a given element.
-- If not, it adds the given failure to the result and validation continues.
hasElem :: (Foldable t, Eq a) => a -> f -> ValueCtx (t a) -> VCtx f (ValueCtx (t a))
hasElem e f = disputeWith $ bool (Just f) Nothing . elem e

-- | Checks that a 'Foldable' does not have a given element.
-- If it has element, it adds the given failure to the result and validation continues.
doesNotHaveElem :: (Foldable t, Eq a) => a -> f -> ValueCtx (t a) -> VCtx f (ValueCtx (t a))
doesNotHaveElem e f = disputeWith $ bool (Just f) Nothing . not . elem e

-- | If any element is valid, the entire value is valid.
ifAny :: (a -> Maybe f) -> ValueCtx [a] -> VCtx f (ValueCtx [a])
ifAny fn v =
  let
    xs = getValue v
    fs = catMaybes $ fmap fn xs
  in if length fs == length xs
    then disputeMany v fs
    else pure v

-- | Every element must be valid.
ifAll :: (a -> Maybe f) -> ValueCtx [a] -> VCtx f (ValueCtx [a])
ifAll fn v = case catMaybes . fmap fn $ getValue v of
  [] -> pure v
  fs -> disputeMany v fs

-- | Validate each element with a given function.
ifEach :: (a -> Either f b) -> ValueCtx [a] -> VCtx f (ValueCtx [b])
ifEach fn v =
  let es = fmap fn $ getValue v
  in case lefts es of
    []  -> pure . setValue v $ rights es
    fs -> refuteMany v fs

-- | Validate each element with a given function.
ifEachProven :: (a -> Proof f b) -> ValueCtx [a] -> VCtx f (ValueCtx [b])
ifEachProven fn v =
  let p = fold $ fmap (fmap (\b -> [b]) . fn) $ getValue v
  in case p of
    Valid es -> pure $ setValue v es
    Invalid gfs lfs -> case v of
      Global _  -> RefutedCtx gfs lfs
      Field n _ -> RefutedCtx [] $ insert [n] gfs lfs

-- | Checks that two fields are equal.
-- If not, it adds the given failure to the result and validation continues.
isMatch :: Eq a => f -> VCtx f a -> ValueCtx a -> VCtx f (ValueCtx a)
isMatch f (ValidCtx a)        = disputeWith (testMatch f a)
isMatch f (DisputedCtx _ _ a) = disputeWith (testMatch f a)
isMatch _ (RefutedCtx _ _)    = pure

{- | Validates a value that implements 'Validatable' and includes any failures under the parent field.

Consider the following example:

@
  data ContactVM = ContactVM { phoneNumber :: String }
  data Contact = ...
  instance Validatable MyFailureType ContactVM Contact where
    ...

  data UserCreatableVM
    = UserCreatableVM
    { userCreatableVMEmailAddress         :: String
    , userCreatableVMConfirmEmailAddress  :: String
    , userCreatableVMPassword             :: String
    , userCreatableVMConfirmPassword      :: String
    , userCreatableVMContact              :: ContactVM
    }
  data UserCreatable = ...

  instance Validatable MyFailureType UserCreatableVM UserCreatable where
    validation u =
      let vc = withField 'userCreatableVMContact (userCreatableVMContact u) $
            validateField                                                      -- (1)
          ...
      in pure UserCreatable \<*> ve \<*> vp \<*> vc <! vce <! vcp
  @

  In line (1), the 'validateField' function uses the 'Validatable' instance on `ContactVM` to validate the type.
  All field specific validation failures are stored in a map where the key is the name of the field.
  However, in this case, there are the fields in the `ContactVM` and the parent field in `UserCreatableVM`.
  These names need to be combined so that the consumer can see if any errors came from nested fields.
  Using the 'validateField' function, any validation failures found in the `ContactVM` value have field names that include the parent field.
  A `ContactVM` with an invalid phone number might have a result like this: `Invalid [] [(['phoneNumber], [InvalidPhoneNumber])]` where `['phoneNumber]` is the key to the map.
  The 'validationField' merges this with the `UserCreatable` result to create something like this: `Invalid [] [(['contact, 'phoneNumber], [InvalidPhoneNumber])]`.
  This allows the consumer to determine exactly what field caused the failure.
-}
validateField :: Validatable f a b => ValueCtx a -> VCtx f (ValueCtx b)
validateField (Global a) = case validation a of
  ValidCtx b            -> ValidCtx (Global b)
  DisputedCtx gfs lfs b -> DisputedCtx gfs lfs (Global b)
  RefutedCtx gfs lfs    -> RefutedCtx gfs lfs
validateField (Field n a) = case validation a of
  ValidCtx b            -> ValidCtx (Field n b)
  DisputedCtx [] lfs b -> DisputedCtx [] (mapKeys (\k -> [n] ++ k) lfs) (Field n b)
  DisputedCtx gfs lfs b -> DisputedCtx [] (insert [n] gfs $ mapKeys (\k -> [n] ++ k) lfs) (Field n b)
  RefutedCtx [] lfs    -> RefutedCtx [] (mapKeys (\k -> [n] ++ k) lfs)
  RefutedCtx gfs lfs    -> RefutedCtx [] (insert [n] gfs $ mapKeys (\k -> [n] ++ k) lfs)

-- | Allows for validation of an optional value.
-- See `Validating Complex Types` for an example.
optional :: Maybe a -> (a -> VCtx f b) -> VCtx f (Maybe b)
optional Nothing _  = ValidCtx Nothing
optional (Just a) f =
  case f a of
    ValidCtx b            -> ValidCtx (Just b)
    DisputedCtx gfs lfs b -> DisputedCtx gfs lfs (Just b)
    RefutedCtx gfs lfs    -> RefutedCtx gfs lfs

-- | tests if a 'Proof' is valid.
isValid :: Proof f a -> Bool
isValid (Valid _)     = True
isValid (Invalid _ _) = False

-- | tests if a 'Proof' is invalid.
isInvalid :: Proof f a -> Bool
isInvalid = not . isValid

-- | Flatten a list of proofs into a proof of the list
flattenProofs :: [Proof f a] -> Proof f [a]
flattenProofs al = mconcat $ fmap (:[]) <$> al
