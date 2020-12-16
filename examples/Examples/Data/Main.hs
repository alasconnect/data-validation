module Main where

------------------------------------------------------------------------------------------------------------------------
import Prelude
import Data.Validation
------------------------------------------------------------------------------------------------------------------------
import Examples.Data.ComplexTypes
import Examples.Data.Primitives
------------------------------------------------------------------------------------------------------------------------

-- make some space
header :: IO ()
header = do
  putStrLn "\n"
  putStrLn "Basic data-validation usage example:"
  putStrLn "\n"

-- only prints contents of valid proof
printValid :: Proof MyFailures User -> IO ()
printValid (Valid user) = do
  putStrLn "--- Valid User ---"
  putStrLn $ show user
  putStrLn "\n"
printValid _ = pure ()

-- only prints invalid proof
printInvalid :: Proof MyFailures User -> IO ()
printInvalid (Valid _) = pure ()
printInvalid failure = do
  putStrLn "--- Invalid Proof ---"
  putStrLn $ show failure
  putStrLn "\n"

-- very basic usage example showcasing types defined in ComplexTypes and Primitives modules
main :: IO ()
main = do
  header
      -- populate forms
  let validForm = UserVM (Just "Hugh Mann") (Just "person@earth.com") Nothing (Just Email) Nothing
      invalidForm = UserVM (Just "broken") Nothing (Just "123") (Just Email) Nothing
      -- validate proofs
      userProof = validate validForm
      userFailure = validate invalidForm
  -- prints user
  printValid userProof
  -- prints nothing
  printInvalid userProof
  -- prints failure
  printInvalid userFailure
  -- prints nothing
  printValid userFailure
