module Main where

import           Hedgehog

main :: IO Bool
main = checkParallel $ Group "Always.Pass" []
