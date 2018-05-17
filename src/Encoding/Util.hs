{-# LANGUAGE ViewPatterns #-}

module Encoding.Util where

import qualified Data.IntMap as M
import           Data.IntMap (IntMap)
import qualified Data.Vector as V
import           Data.Vector (Vector)
import           Data.Tuple
import           Data.List (elemIndex)
import           Data.Char (toUpper, ord)

import Encoding.Types

zipWithPrev :: [[Int]] -> Vector (Vector (Int,Int))
zipWithPrev lst = V.fromList . map V.fromList $ routine [0..] lst
  where
    -- to zip the first rotor
    routine keyboard (this:next) = zip this keyboard : go this next
    go _ [] = []
    go prev (this:next) = zipByIndex 0 this prev : go this next
    zipByIndex _ [] _ = []
    zipByIndex n (x:xs) prev = (x, unsafeElemIndex n prev) : zipByIndex (n+1) xs prev
    unsafeElemIndex x xs = case elemIndex x xs of
      Nothing -> error "unsafeElemIndex: no element found"
      Just n -> n

makeRotors :: Vector (Vector Connection) -> Vector Part
makeRotors conn = do
  (int,con) <- V.zip (V.enumFromN 1 (V.length conn)) conn
  return . Right $ Rotor int 0 con

doubleMirror :: [(a,a)] ->  [(a,a)]
doubleMirror = concatMap (\x -> [x, swap x])

deconst :: V.Vector Part -> (Part, Vector Part)
deconst = (,) <$> V.head <*> V.tail

numberify :: String -> [Int]
numberify = map (subtract 65 . ord . toUpper)