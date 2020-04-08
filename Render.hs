module Render where

import Tools

import Graphics.Gloss

itof :: Int -> Float
itof = toEnum . fromIntegral

grid :: Int -> Float -> Float -> Picture
grid cels width height = pictures $ hori ++ vert
  where
    coords a = take (cels-1) $ map (*(a/cels')) [1..]
    hori   = map (\y -> translate (width/2) y $ color white $ rectangleSolid width 2) $ coords height
    vert   = map (\x -> translate x (height/2) $ color white $ rectangleSolid 2 height) $ coords width
    cels'  = itof cels

render :: (Int, Int) -> Model -> Picture
render screenSize@(width, height) (objs, time, _) = pictures [gridPic, dotPic, textPic, origin]
  where
    -- General data
    width'  = itof width
    height' = itof height
    time'   = itof time
    dtx     = width'/(time'*2)
    dty     = height'/(time'*2)
    -- Components
    gridPic = translate (-width'/2) (-height'/2) $ grid (time*2) width' height'
    dotPic  = pictures $ map renderDot objs
    renderDot (m, (x, y)) = translate (snap x dtx) (snap y dty) $ color white $ circle (itof m * 10)
    textPic = pictures $ map renderText objs
    renderText (m, (x, y)) = if m <= 1 then Blank else
      translate (snap x dtx) (snap y dty) $ color white $ Graphics.Gloss.text $ show m
    origin = translate (snap 0 dtx) (snap 0 dty) $ color red $ circle 20
    -- Helpers
    snap c dtc = (itof c + 0.5) * dtc

