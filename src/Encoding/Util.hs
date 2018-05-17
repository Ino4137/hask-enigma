{-# LANGUAGE ViewPatterns, ParallelListComp #-}

module Encoding.Util where

import qualified Data.IntMap as IM
import           Data.IntMap (IntMap, Key)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Tuple
import           Data.List (elemIndex)
import           Data.Char (toUpper, ord)

import Encoding.Types

makeRotors :: Vector [Connection] -> Vector Part
makeRotors conn = do
  (int,con) <- V.zip (V.enumFromN 1 (V.length conn)) conn
  return . Right $ Rotor int 0 (IM.fromList $ connections con)
    where
      connections lst = [
          (i,xy)
            | i <- [0..]
            | xy <- lst
        ]

doubleMirror :: [(a,a)] ->  [(a,a)]
doubleMirror = concatMap (\x -> [x, swap x])

deconst :: V.Vector Part -> (Part, Vector Part)
deconst = (,) <$> V.head <*> V.tail

numberify :: String -> [Int]
numberify = map (subtract 65 . ord . toUpper)