module Main where

import Prelude

import Data.Maybe
import Data.List
import Data.Int (toNumber)

import Control.Monad.Eff
import Control.Monad.Eff.Console (error, log, CONSOLE)

import Graphics.Canvas

import Signal.DOM (keyPressed, windowDimensions, DimensionPair)
import Signal (filter, runSignal, Signal, foldp, sampleOn, map2)
import Signal.Time (every)
import Math

type Planet =
  { x :: Number
  , y :: Number
  , r :: Number
  , color :: String
  }

update :: List Planet -> Boolean -> List Planet
update planets false = planets
update planets true = Cons {x: 10.0, y: 10.0, r: 15.0, color: "rgba(0,0,0,0.1)"} planets

frameRate :: Signal Number
frameRate = every 33.0

data Scene = Scene (List Planet) DimensionPair

scene :: Signal Boolean -> Signal DimensionPair -> Signal Scene
scene spaces dimens =
  map2 Scene planets dimens
  where planets = foldp (flip update) Nil spaces

renderPlanets :: forall eff. Context2D -> List Planet -> (Eff (canvas :: Canvas | eff) Context2D)
renderPlanets ctx Nil = do
  return ctx
renderPlanets ctx (Cons planet scene) = do
  ctx <- circle ctx planet
  renderPlanets ctx scene

renderScene :: forall eff. Context2D -> Scene -> (Eff (canvas :: Canvas | eff) Unit)
renderScene ctx (Scene planets dimens) = do
  clearRect ctx { x: 0.0
                , y: 0.0
                , w: toNumber dimens.w
                , h: toNumber dimens.h
                }
  renderPlanets ctx planets
  return unit

circle ctx c = do
  ctx <- setFillStyle c.color ctx
  ctx <- arc ctx {x: c.x, y: c.y, r: c.r, start: 0.0, end: Math.pi * 2.0}
  fill ctx

pressedSpace :: forall eff. Boolean -> (Eff (console :: CONSOLE | eff) Unit)
pressedSpace true = do
  log "pressed space!"
pressedSpace false = do
  return unit

setCanvasSize :: forall eff. CanvasElement -> DimensionPair -> (Eff (canvas :: Canvas | eff) Unit)
setCanvasSize canvas {w, h} = do
  setCanvasWidth (toNumber w) canvas
  setCanvasHeight (toNumber h) canvas
  return unit

main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> error "No canvas"
    Just canvasElement -> do
      ctx <- getContext2D canvasElement
      spaces <- keyPressed 32
      dimens <- windowDimensions
      let scene' = scene (sampleOn frameRate spaces) dimens
      runSignal $ map (renderScene ctx) scene'
      runSignal $ map pressedSpace spaces
      runSignal $ map (setCanvasSize canvasElement) dimens
      -- runSignal spaceEffect
