module Main where

import Lib
import Types

main :: IO ()
main = do
  let thetas = Coefficients (0, 0)
  let alpha = 0.3
  let trainingset =
        TrainingSet [Example (0, 50), Example (1, 60), Example (2, 70)]
  let iterations = 500
  print $ show $ linearRegression thetas alpha trainingset iterations
