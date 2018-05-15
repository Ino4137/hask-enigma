module Encoding.Types where

import Data.Vector (Vector)
import Data.IntMap (IntMap)

data Connection = Connection {
  forward :: Word,
  backward :: Word
} deriving Show

data Rotor = Rotor {
  -- How often the rotor will move on its own, also it's "number"
  interval :: Word,
  -- clock that once reaches 26 pushes rotor behind it a rotation further
  clock :: Word,
  state :: Vector Connection
} deriving Show

newtype Reflector = Reflector {
  -- used IntMap for the ease of use
  stateR :: IntMap Word
} deriving Show

data From = 
  F | B 
  deriving (Eq, Show)

-- Should (Vector Part) be a (ExceptT Reflector Vector Rotor)?
type Part = Either Reflector Rotor

data Enigma = Enigma {
  -- amount of rotors
  rotorsN :: Word,
  -- starting positions, with 0 as a no-offset
  sPositions :: Vector Word,
  -- the whole machinery
  rotors :: Vector Part,
  -- redirecting bidirectionally
  swaps :: IntMap Word
} deriving Show

