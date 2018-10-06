module Types where

newtype Coefficients =
  Coefficients (Float, Float)
  deriving (Show)

newtype Example =
  Example (Float, Float)

newtype TrainingSet =
  TrainingSet [Example]
