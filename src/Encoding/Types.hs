module Encoding.Types where

import Data.Vector (Vector)
import Data.IntMap (IntMap)

data Connection = Connection {
  forward :: Int,
  backward :: Int
} deriving Show

data Rotor = Rotor {
  -- How often the rotor will move on its own, also it's "number"
  interval :: Int,
  -- clock that once reaches 26 pushes rotor behind it a rotation further
  clock :: Int,
  state :: Vector Connection
} deriving Show

newtype Reflector = Reflector {
  -- used IntMap for the ease of use
  stateR :: IntMap Int
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
  swaps :: IntMap Int
} deriving Show

