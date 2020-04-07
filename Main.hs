module Main where

import Movement
import Collisions
import Tools
import Modulus

import Graphics.Gloss
import Graphics.Gloss.Interface.Environment (getScreenSize)

import Debug

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
  let launch = [
        (1, (-2, -2)),
        (3, (-3, -3)),
        (3, (2, 2)),
        (1, (1, -2)),
        (3, (2, -3)),
        (3, (-3, 2))
        ]
      time = 3
      model = (launch, time)
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
    hori   = map (\y -> translate (width/2) y $ color white $ rectangleSolid width 5) $ coords height
    vert   = map (\x -> translate x (height/2) $ color white $ rectangleSolid 5 height) $ coords width
    cels'  = itof cels

render :: (Int, Int) -> Model -> Picture
render screenSize@(width, height) (objs, time) = pictures [gridPic, dots]
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
    -- Helpers
    snap c dtc = (itof c + 0.5) * dtc
