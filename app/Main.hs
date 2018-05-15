module Main where

import Encoding
import qualified Data.Vector as V
import qualified Data.IntMap as M

main :: IO ()
main = do
  putStrLn "The enigma:"
  print parts 
  putStrLn "Testing encoding A -> B"
  putChar $ encodeCharacter 'A' parts
  putStrLn "\nAnd the other way round"
  putChar $ encodeCharacter 'B' parts
  putChar '\n'
    where
      stats = V.fromList $ map V.fromList $ replicate 3 (zipWith Connection [0..25] [0..25])
      rotorIT (n,vec) = Right $ Rotor n 99 vec
      rotors = V.map rotorIT (V.zip (V.fromList [1,2,3]) stats)
      refl = Left . Reflector $ M.fromList [(0,1), (1,0)] -- A -> B, B -> A
      parts :: V.Vector Part
      parts = V.snoc rotors refl


