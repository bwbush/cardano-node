module Main (main) where

import           Control.Monad (unless)
import           System.Exit (exitFailure)
import qualified Test.Cardano.Tracing.NewTracing.TransactionLogging as TransactionLogging

main :: IO ()
main = do
  passed <- TransactionLogging.tests
  unless passed exitFailure
