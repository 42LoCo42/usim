-- vim: shiftwidth=2:expandtab
{-# LANGUAGE OverloadedStrings #-}
module Main where

import           Control.Monad   (when)
import qualified Data.Map.Strict as M
import           Data.Text       (Text, pack)
import           Foreign.C       (CInt)
import qualified Linear          as L
import           SDL             (($=))
import qualified SDL
import qualified SDL.Font

import Universe
import Utils

scale :: CInt
scale = 50

test :: M.Map Vector (Int, Vector)
test = M.fromList
  [ (Vector 0 0, (1, Vector 7 0))
  , (Vector 3 0, (3, Vector 0 0))
  , (Vector 5 0, (5, Vector 0 0))
  , (Vector 2 0, (2, Vector 0 0))
  , (Vector 2 1, (9, Vector 1 1))
  ]

mtest :: M.Map Vector Int
mtest = M.fromList
  [ (Vector 0 0, 1)
  , (Vector 0 2, 1)
  , (Vector 0 4, 1)
  , (Vector 1 1, 1)
  ]

rotate :: SDL.V2 CInt -> Double -> SDL.V2 CInt -> SDL.V2 CInt
rotate around angle point = rotated
  where
    (L.V2 relX relY)       = point - around
    quarternion            = L.axisAngle (L.V3 0 0 1) angle
    raw@(L.V3 rawX rawY 0) = L.rotate quarternion (L.V3 (c relX) (c relY) 0)
    rawLength              = sqrt (rawX ** 2 + rawY ** 2)
    (L.V3 rotX rotY 0)     = (c scale * 0.25 / rawLength) L.*^ raw
    rotated                = L.V2 (c rotX) (c rotY) + around

drawTextCentered ::
  SDL.Renderer -> SDL.Font.Font
  -> Text -> CInt -> CInt -> IO ()
drawTextCentered renderer font text x y = do
  surface <- SDL.Font.blended font (SDL.V4 255 255 255 255) text
  (SDL.V2 width height) <- SDL.surfaceDimensions surface
  texture <- SDL.createTextureFromSurface renderer surface
  SDL.copy renderer texture Nothing $ Just $ SDL.Rectangle
    (SDL.V2 (x - width `div` 2) (y - height `div` 2) % SDL.P)
    (SDL.V2 width height)

drawArrow :: SDL.Renderer -> Int -> Int -> Int -> Int -> IO ()
drawArrow _        _ _ 0  0  = return ()
drawArrow renderer x y dx dy = do
  SDL.drawLine renderer startPoint endPoint
  SDL.drawLine renderer endPoint (SDL.V2 (c e1x) (c e1y) % SDL.P)
  SDL.drawLine renderer endPoint (SDL.V2 (c e2x) (c e2y) % SDL.P)
  where
    cx         = c x * scale + scale `div` 2
    cy         = c y * scale + scale `div` 2
    startV     = SDL.V2 cx cy
    startPoint = SDL.P startV

    cex      = cx + c dx * scale
    cey      = cy + c dy * scale
    endV     = SDL.V2 cex cey
    endPoint = SDL.P endV

    endBase        = L.V2 (c cx) (c cy)
    (L.V2 e1x e1y) = rotate endV ( pi/12) endBase
    (L.V2 e2x e2y) = rotate endV (-pi/12) endBase

renderPoints :: SDL.Renderer -> SDL.Font.Font -> M.Map Vector Int -> IO ()
renderPoints r font m = do
  sequence_ $ M.mapWithKey renderPoint m
  where
    renderPoint :: Vector -> Int -> IO ()
    renderPoint (Vector x y) mass = do
      edgeRect % SDL.drawRect r
      drawTextCentered r font (show mass % pack) centerX centerY
      where
        cornerX   = c x * scale
        cornerY   = c y * scale

        centerOff = scale `div` 2
        centerX   = cornerX + centerOff
        centerY   = cornerY + centerOff

        edgeRect = Just $ SDL.Rectangle
          (SDL.V2 cornerX cornerY % SDL.P)
          (SDL.V2 scale scale)

renderMovingPoints ::
  SDL.Renderer -> SDL.Font.Font
  -> M.Map Vector (Int, Vector) -> IO ()
renderMovingPoints r font m = do
  M.toList  m % map renderMovingPoint % sequence_
  M.map fst m % renderPoints r font
  where
    renderMovingPoint :: (Vector, (Int, Vector)) -> IO ()
    renderMovingPoint (Vector x y, (_, Vector dx dy)) = do
      drawArrow r x y dx dy

startRender :: SDL.Renderer -> IO ()
startRender renderer = do
  SDL.rendererDrawColor renderer $= SDL.V4 0 0 0 255
  SDL.clear renderer
  SDL.rendererDrawColor renderer $= SDL.V4 255 255 255 255

stopRender :: SDL.Renderer -> IO ()
stopRender = SDL.present

main :: IO ()
main = do
  SDL.initializeAll
  SDL.Font.initialize

  font     <- SDL.Font.load "/usr/share/fonts/noto/NotoSans-Regular.ttf" 32
  window   <- SDL.createWindow "usim" SDL.defaultWindow
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  startRender renderer
  stopRender renderer
  appLoop renderer font
  SDL.destroyWindow window

appLoop :: SDL.Renderer -> SDL.Font.Font -> IO ()
appLoop renderer font = do
  events    <- SDL.pollEvents
  continues <- mapM (handleEvent renderer font) events
  when (and continues) (appLoop renderer font)

handleEvent :: SDL.Renderer -> SDL.Font.Font -> SDL.Event -> IO Bool
handleEvent r f e = case SDL.eventPayload e of
  SDL.WindowClosedEvent _ -> return False
  SDL.KeyboardEvent kev -> do
    when (SDL.keyboardEventKeyMotion kev == SDL.Pressed) $ do
      let key = SDL.keyboardEventKeysym kev % SDL.keysymKeycode
      case key of
        SDL.KeycodeM -> do
          startRender r
          renderMovingPoints r f test
          stopRender r
        SDL.KeycodeS -> do
          startRender r
          collisions test % movePoints % renderPoints r f
          stopRender r
        _ -> return ()
    return True
  _ -> return True
