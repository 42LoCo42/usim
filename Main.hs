module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)
import Graphics.Gloss.Interface.IO.Interact
import Initial
import Render
import System.Random (getStdGen)
import Tools
import Update

main :: IO ()
main = do
  screenSize <- getScreenSize
  gen <- getStdGen
  let time = 6
      model = (initial, time, gen)
  simulate
    FullScreen
    black
    1
    model
    (render screenSize)
    (\_ _ -> update)

disp :: Model -> IO ()
disp model = display FullScreen black (render (800, 800) model)

dispInitial :: IO ()
dispInitial = do
  gen <- getStdGen
  disp (uCycle 6 initial, 6, gen)
