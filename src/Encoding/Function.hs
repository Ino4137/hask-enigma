{-# LANGUAGE RecordWildCards, ViewPatterns #-}

module Encoding.Function where

import qualified Data.Map as M
import           Data.Map (Map)
import qualified Data.IntMap as IM
import           Data.IntMap (IntMap)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Char (chr, ord, toUpper)
import           Data.Maybe (fromMaybe)
import           Data.Either (fromRight)
import           Data.Maybe (fromJust)
import           Data.List (sortOn)
import           Control.Monad.Trans.State
import           Data.Tuple (swap)

import           Encoding.Types
import           Encoding.Std
import           Encoding.Util (deconst)

encodeString :: Enigma -> String -> String
encodeString Enigma{..} code = uncode . V.foldr ((:)) "" . snd $ execState (mapM encodeCharacter $ uncode code) (rotors, V.empty)
  where 
    -- Swapping the letters before and after encoding
    uncode = foldr (\x acc -> case M.lookup x swaps of
        Nothing -> x:acc
        Just x' -> x':acc 
      ) ""

encodeCharacter :: Char -> State (Vector Part, Vector Char) ()
encodeCharacter char = do
  (parts, code) <- get
  -- movement of rotors happens after encoding
  put (rotorsNew code parts, V.snoc code $ encodeCharacterRaw parts char)
    where
      rotorsNew code = tick ((== 0) . rem (V.length code + 1))
      tick :: (Int -> Bool) -> Vector Part -> Vector Part
      tick f vec = V.foldr (\r acc -> case r of 
          Left x -> acc
          Right Rotor{..} -> if f interval 
            then rotateRotorN interval acc
            else acc
        ) vec vec

encodeCharacterRaw :: Vector Part -> Char -> Char
encodeCharacterRaw rotors c = go (subtract 65 . ord $ toUpper c) F rotors
  where
    --getIndex a = V.findIndex ((==a) . backward) (unRotor V.head rotors) 
    go :: Int -> From -> Vector Part -> Char
    -- if it reached the reflector, turn back
    go n F (deconst -> (Left Reflector{..}, _)) = go (stateRef IM.! n) B (prepareForBackwards rotors)
    -- step forward
    go n F (deconst -> (Right Rotor{..}, xs)) = go (stateRot IM.! n) F xs
    -- if the rank is 1, means we hit the first rotor
    go n B rot@(checkRank . V.last -> 1) = chr $ ((unRotor V.last rot) IM.! n) + 65
    -- step backwards
    go n B xs@(V.last -> Right Rotor{..}) = go (stateRot IM.! n) B (V.init xs)
    
    -- it just works. 
    prepareForBackwards :: Vector Part -> Vector Part
    prepareForBackwards = V.fromList . map (Right . updateS (IM.fromList . map swap . IM.toList) . fromRight (error "")) . V.toList . V.init
    unRotor f rot = case f rot of 
      Right Rotor{..} -> stateRot
      _ -> error "unRotor: part was a reflector"

    updateS f Rotor{stateRot=f->stateRot,..} = Rotor{..}
        
-- TODO: make it offset rotors to the starting position
buildEnigma :: Maybe Int -> Maybe (Vector Int) -> Maybe (Map Char Char)
      -> Maybe Reflector -> Enigma
buildEnigma amm offsets swaps refl = Enigma rotorsN 
  (fromMaybe stdOffsets offsets)
  (V.snoc (V.take rotorsN stdRotors) reflector)
  (fromMaybe stdSwaps swaps)
    where 
      rotorsN = fromMaybe stdAmm amm
      reflector :: Part
      reflector = Left $ fromMaybe stdRefl refl

buildStdEnigma :: Enigma
buildStdEnigma = buildEnigma Nothing Nothing Nothing Nothing

checkRank :: Part -> Int
checkRank (Left _) = 0
checkRank (Right Rotor{..}) = interval

-- N stands for the interval
-- also updates the next one's backward pointing,
-- checks for double-rotate and rotates if needed
-- (messy but it just werks)
rotateRotorN :: Int -> Vector Part -> Vector Part
rotateRotorN n rotors = 
  let rotor1@(Rotor inte clock stateRot) = updateForward $ unPart (n-1)
      -- if the clock hits the new rotation, rotate the previous rotor
      accForClock = if inte > 2 && ((clock+1) `rem` 26 == 0) then
          rotateRotorN (n-1) $ reInsert (n-1) (V.singleton . Right $ moveClock rotor1) rotors
        else reInsert (n-1) (V.singleton . Right $ moveClock rotor1) rotors
      -- update the next rotor's backward connection if it isn't a reflector
      rotorsNew = if V.length rotors - n /= 1 then
          reInsert n (V.singleton $ Right (updateBackward $ unPart n)) accForClock
        else accForClock
  in rotorsNew
    where
      reInsert n parts vec = V.take (n-1) vec V.++ parts V.++ V.drop (V.length parts) vec
      unPart n = fromRight (error "unPart: hit reflector") (rotors V.! (n-1))

updateBackward :: Rotor -> Rotor
updateBackward Rotor{..} = Rotor{stateRot= fmap f stateRot,..}
  where
    f val = (val-1) `mod` 26

moveClock :: Rotor -> Rotor
moveClock rotor = rotor { interval = ((interval rotor +1) `rem` 26)}

updateForward :: Rotor -> Rotor
updateForward Rotor{..} = Rotor{stateRot= IM.mapKeys f stateRot,..}
  where
    f val = (val+1) `mod` 26