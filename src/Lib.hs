module Lib where

import Data.List
import Types

-- Finds coefficients for linear regression using gradient descent
linearRegression :: Coefficients -> Float -> TrainingSet -> Int -> Coefficients
linearRegression coefficients alpha dataset iterations
  | iterations == 0 = coefficients
  | otherwise =
    let thetas = newThetas coefficients alpha dataset
     in linearRegression thetas alpha dataset (iterations - 1)

-- Calculate new values for t0 and t1
newThetas :: Coefficients -> Float -> TrainingSet -> Coefficients
newThetas thetas alpha dataset =
  let deltas = map (calculateDelta thetas) examples
      adjustedDeltas = adjustDeltas deltas examples
      newt0 = t0 - alpha * avg deltas
      newt1 = t1 - alpha * avg adjustedDeltas
   in Coefficients (newt0, newt1)
  where
    Coefficients (t0, t1) = thetas
    TrainingSet examples = dataset

-- Calculates the difference between h(x) and y
calculateDelta :: Coefficients -> Example -> Float
calculateDelta thetas example = t0 + t1 * x - y
  where
    Coefficients (t0, t1) = thetas
    Example (x, y) = example

-- For the case where d/dx != 1 and we need to multiply through, do so 
adjustDeltas :: [Float] -> [Example] -> [Float]
adjustDeltas deltas examples =
  let xs = map (\(Example (x, _)) -> x) examples
      zipped = zip deltas xs
   in map (uncurry (*)) zipped

avg xs = realToFrac (sum xs) / genericLength xs
