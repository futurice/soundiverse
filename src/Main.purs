module Main where

import Prelude

import Data.Maybe
import Data.List

import Control.Monad.Eff
import Control.Monad.Eff.Console (error, log, CONSOLE)

import Graphics.Canvas

import Signal.DOM (keyPressed)
import Signal (filter, runSignal, Signal, foldp, sampleOn)
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

scene :: Signal Boolean -> Signal (List Planet)
scene spaces = foldp (flip update) Nil spaces

renderScene :: forall eff. Context2D -> List Planet -> (Eff (canvas :: Canvas | eff) Context2D)
renderScene ctx Nil = do
  return ctx
renderScene ctx (Cons planet scene) = do
  ctx <- circle ctx planet
  renderScene ctx scene

renderScene' :: forall eff. Context2D -> List Planet -> (Eff (canvas :: Canvas | eff) Unit)
renderScene' ctx planets = do
  clearRect ctx {x: 0.0, y: 0.0, w: 100.0, h: 100.0}
  renderScene ctx planets
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


main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> error "No canvas"
    Just canvasElement -> do
      ctx <- getContext2D canvasElement
      spaces <- keyPressed 32
      let scene' = scene $ sampleOn frameRate spaces
      runSignal $ map (renderScene' ctx) scene'
      runSignal $ map pressedSpace spaces
      -- runSignal spaceEffect
