{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Encoding.Function where

import qualified Data.IntMap as M
import           Data.IntMap (IntMap)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Char (chr, ord, toUpper)
import           Data.Maybe (fromMaybe)

import           Encoding.Types
import           Encoding.Std

encodeCharacter :: Char -> Vector Part -> Char
encodeCharacter c rotors = go (fromIntegral . subtract 65 . ord $ toUpper c) F rotors
  where
    go :: Word -> From -> Vector Part -> Char
    -- if it reached the reflector, turn back
    go n F (deconst -> (Left Reflector{..}, _)) = go (stateR M.! fromIntegral n) B (V.init rotors)
    -- step forward
    go n F (deconst -> (Right Rotor{..}, xs)) = go (forward $ state V.! fromIntegral n) F xs
    -- if the rank is 1, means we hit the first rotor
    go n B (checkRank . V.last -> 1) = chr (fromIntegral n + 65)
    -- step backwards
    go n B xs@(V.last -> Right Rotor{..}) = go (backward $ state V.! fromIntegral n) B (V.init xs)


-- TODO: make it offset rotors to the starting position
buildEnigma :: Maybe Word -> Maybe (Vector Word) -> Maybe (IntMap Word)
      -> Maybe Reflector -> Enigma
buildEnigma amm offsets swaps refl = Enigma rotorsN 
  (fromMaybe stdOffsets offsets)
  (V.snoc (V.take (fromIntegral rotorsN) stdRotors) reflector)
  (fromMaybe stdSwaps swaps)
    where 
      rotorsN = fromMaybe stdAmm amm
      reflector :: Part
      reflector = Left $ fromMaybe stdRefl refl

buildStdEnigma :: Enigma
buildStdEnigma = buildEnigma Nothing Nothing Nothing Nothing

deconst :: V.Vector Part -> (Part, Vector Part)
deconst = (,) <$> V.head <*> V.tail

checkRank :: Part -> Word
checkRank (Left _) = 0
checkRank (Right Rotor{..}) = interval