module Encoding.Util where

import qualified Data.IntMap as M
import           Data.IntMap (IntMap)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Tuple

import Encoding.Types

zipWithPrev :: Num a => [[a]] -> Vector (Vector (a,a))
zipWithPrev lst = V.fromList . map V.fromList $ go (cycle [0]) lst
  where
    go _ [] = []
    go prev (this:next) = zip this prev : go this next

makeRotors :: Vector (Vector Connection) -> Vector Part
makeRotors conn = do
  (int,con) <- V.zip (V.enumFromN 1 (V.length conn)) conn
  return . Right $ Rotor int 0 con

doubleMirror :: [(a,a)] ->  [(a,a)]
doubleMirror = concatMap (\x -> [x, swap x])