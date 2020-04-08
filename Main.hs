module Main where

import Movement
import Collisions
import Tools
import Modulus
import Initial

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)


type Model = ([Object], Int)

itof :: Int -> Float
itof = toEnum . fromIntegral

uCycle :: Int -> [Object] -> [Object]
uCycle t objs =
  staticCol $ map (modulObj max min) $ finishMove $ linCols
  where
    linCols = map (linearCol withVec) withVec
    withVec = zip objs $ moveAll objs
    max     = t-1
    min     = -t

main = do
  let time = 6
      model = (initial, time)
  screenSize <- getScreenSize
  simulate
    FullScreen
    black
    1
    model
    (render screenSize)
    (\_ -> \_ -> \(objs, time) -> (uCycle (time) objs, time+1))

grid :: Int -> Float -> Float -> Picture
grid cels width height = pictures $ hori ++ vert
  where
    coords a = take (cels-1) $ map (*(a/cels')) [1..]
    hori   = map (\y -> translate (width/2) y $ color white $ rectangleSolid width 2) $ coords height
    vert   = map (\x -> translate x (height/2) $ color white $ rectangleSolid 2 height) $ coords width
    cels'  = itof cels

render :: (Int, Int) -> Model -> Picture
render screenSize@(width, height) (objs, time) = pictures [gridPic, dots, texts]
  where
    -- General data
    width'  = itof width
    height' = itof height
    time'   = itof time
    dtx     = width'/(time'*2)
    dty     = height'/(time'*2)
    -- Components
    gridPic = translate (-width'/2) (-height'/2) $ grid (time*2) width' height'
    dots    = pictures $ map dot objs
    dot (m, (x, y)) = translate (snap x dtx) (snap y dty) $ color white $ circle (itof m * 10)
    texts   = pictures $ map text objs
    text (m, (x, y)) = translate (snap x dtx) (snap y dty) $ color white $ Graphics.Gloss.text $ show m
    -- Helpers
    snap c dtc = (itof c + 0.5) * dtc
