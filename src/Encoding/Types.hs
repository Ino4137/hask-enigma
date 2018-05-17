module Encoding.Types where

import Data.Vector (Vector)
import Data.IntMap (IntMap)
import Data.Map    (Map)

type Connection = (Int,Int)

data Rotor = Rotor {
  -- How often the rotor will move on its own, also it's "number"
  interval :: Int,
  -- clock that once reaches 26 pushes rotor behind it a rotation further
  clock :: Int,
  stateRot :: IntMap Connection
} deriving Show

newtype Reflector = Reflector {
  -- used IntMap for the ease of use
  stateRef :: IntMap Int
} deriving Show

data From = 
  F | B 
  deriving (Eq, Show)

-- Should (Vector Part) be a (ExceptT Reflector Vector Rotor)?
type Part = Either Reflector Rotor

data Enigma = Enigma {
  -- amount of rotors
  rotorsN :: Int,
  -- starting positions, with 0 as a no-offset
  sPositions :: Vector Int,
  -- the whole machinery
  rotors :: Vector Part,
  -- redirecting bidirectionally
  swaps :: Map Char Char
} deriving Show

