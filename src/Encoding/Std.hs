module Encoding.Std where

import qualified Data.IntMap as M
import           Data.IntMap (IntMap)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Bifunctor

import Encoding.Types
import Encoding.Util

stdAmm :: Int
stdAmm = 8

-- meaningless numbers
stdOffsets :: Vector Int
stdOffsets = V.fromList [3,1,5,1,5,9,2,6,5,3,5,8,9,7,9,3]

stdRotors :: Vector Part
stdRotors = makeRotors . (V.map . V.map) (uncurry Connection) . zipWithPrev $
  [
    [13,5,9,20,22,14,2,0,12,25,16,7,10,19,11,4,24,8,21,15,1,6,3,18,23,17],
    [4,9,10,7,1,5,11,6,3,8,2,0,12,13,14,15,16,17,18,19,20,21,22,23,24,25],
    [22,25,7,14,9,10,20,15,6,1,24,17,21,3,18,2,23,16,11,12,8,5,13,19,0,4],
    [20,15,18,8,6,2,0,25,3,11,9,16,13,5,14,23,4,19,12,24,7,21,17,1,22,10],
    [12,10,1,18,7,2,17,24,22,14,8,6,13,23,20,15,4,19,11,0,25,9,21,3,16,5],
    [11,19,6,4,23,14,18,5,13,20,24,3,16,7,25,2,9,10,22,8,0,21,15,12,17,1],
    [7,22,1,10,24,23,25,14,21,19,6,9,0,11,3,8,13,18,20,5,2,16,4,12,17,15],
    [22,0,24,19,18,15,13,12,7,8,10,9,17,21,5,14,25,23,6,2,1,16,20,11,3,4],
    [0,25,16,19,10,1,20,4,17,2,24,3,21,18,11,13,6,8,15,7,22,9,23,5,12,14],
    [0,19,22,21,23,25,2,15,3,14,24,1,6,5,17,4,20,13,18,8,16,11,7,12,10,9],
    [25,10,18,23,0,7,13,8,21,17,11,3,24,14,16,9,20,4,22,15,19,6,1,12,2,5],
    [1,2,19,8,12,11,15,10,25,9,7,0,21,24,3,18,17,13,14,20,22,5,16,6,23,4]
  ] 

-- couldn't figure out how to make doubleMirror work perfectly so here's a hack
stdSwaps :: IntMap Int
stdSwaps = M.fromList . map (bimap fromIntegral fromIntegral) . doubleMirror $
  [
    (7,13),
    (4,12), 
    (24,5), 
    (8,17),
    (14,3)
  ]

-- couldn't figure out how to make doubleMirror work perfectly so here's a hack
stdRefl :: Reflector
stdRefl = Reflector . M.fromList . map (bimap fromIntegral fromIntegral) . doubleMirror $
  [
    (9,21),
    (6,23),
    (1,7),
    (25,8),
    (14,12),
    (17,0),
    (3,24),
    (11,22),
    (10,2),
    (15,19),
    (16,5),
    (18,4),
    (20,13)
  ]