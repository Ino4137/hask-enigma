{-# LANGUAGE GeneralizedNewtypeDeriving, RecordWildCards, ViewPatterns #-}

module Encoding.Function where

import qualified Data.IntMap as M
import qualified Data.Vector as V
import           Data.Char (chr, ord, toUpper)
import           Encoding.Types

encodeCharacter :: Char -> V.Vector Part -> Char
encodeCharacter c rotors = go (fromIntegral . subtract 65 . ord $ toUpper c) F rotors
  where
    go :: Word -> From -> V.Vector Part -> Char
    -- if it reached the reflector, turn back
    go n F (deconst -> (Left Reflector{..}, _)) = go (stateR M.! fromIntegral n) B (V.init rotors)
    -- step forward
    go n F (deconst -> (Right Rotor{..}, xs)) = go (forward $ state V.! fromIntegral n) F xs
    -- if the rank is 1, means we hit the first rotor
    go n B (checkRank . V.last -> 1) = chr (fromIntegral n + 65)
    -- step backwards
    go n B xs@(V.last -> Right Rotor{..}) = go (backward $ state V.! fromIntegral n) B (V.init xs)
        
deconst :: V.Vector Part -> (Part, V.Vector Part)
deconst = (,) <$> V.head <*> V.tail

checkRank :: Part -> Word
checkRank (Left _) = 0
checkRank (Right Rotor{..}) = interval