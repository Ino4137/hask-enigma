module Main where

import Encoding
import qualified Data.Vector as V
import qualified Data.IntMap as M

main :: IO ()
main = putStrLn . encodeString buildStdEnigma =<< getLine


