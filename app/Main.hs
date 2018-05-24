module Main where

import Encoding
import qualified Data.Vector as V
import qualified Data.IntMap as M

import Control.Monad
import Control.Monad.IO.Class
import Data.IORef
import Graphics.UI.Gtk hiding (Action, backspace)

mainCmd :: IO ()
mainCmd = putStrLn . encodeString buildStdEnigma =<< getLine

main :: IO ()
main = do
  void initGUI
  window <- windowNew

  widgetShowAll window
  mainGUI

