module Main where

import Prelude

import Data.Maybe
import Data.List
import Data.Int (toNumber)
import Debug.Trace (spy)

import Control.Apply (lift2)
import Control.Monad.Eff
import Control.Monad.Eff.Console (error, log, CONSOLE)
import Control.Monad.Eff.Random (randomInt, randomRange, RANDOM)

import Graphics.Canvas

import Signal.DOM (keyPressed, windowDimensions, DimensionPair)
import Signal (filter, runSignal, Signal, foldp, sampleOn, map2, unwrap)
import Signal.Time (every)
import Math

type Planet =
  { x :: Number
  , y :: Number
  , r :: Number
  , color :: String
  }

randomPlanet :: forall e. DimensionPair -> Eff (random :: RANDOM | e) Planet
randomPlanet {w, h} = do
  r <- randomInt 0 256
  g <- randomInt 0 256
  b <- randomInt 0 256

  x <- randomInt 0 w
  y <- randomInt 0 h
  radius <- randomInt 0 100

  return { x: toNumber x
         , y: toNumber y
         , r: toNumber radius
         , color: "rgb("
                  ++ (show r) ++ ","
                  ++ (show g) ++ ","
                  ++ (show b) ++ ")"
         }

identity :: forall a. a -> a
identity x = x

frameRate :: Signal Number
frameRate = every 33.0

data Scene = Scene (List Planet) DimensionPair

scene :: forall eff. Signal Boolean -> Signal DimensionPair -> Eff (random :: RANDOM | eff) (Signal Scene)
scene spaces dimens = do
  planets <- unwrap $ map2 (\ds _ -> randomPlanet ds) dimens (filter identity false spaces)
  let planetList = foldp Cons Nil planets
  return $ map2 Scene planetList dimens

renderPlanets :: forall eff. Context2D -> List Planet -> (Eff (canvas :: Canvas | eff) Context2D)
renderPlanets ctx Nil = do
  return ctx
renderPlanets ctx (Cons planet scene) = do
  ctx <- circle ctx planet
  renderPlanets ctx scene

blackness :: forall eff. Context2D -> DimensionPair -> (Eff (canvas :: Canvas | eff) Context2D)
blackness ctx {w, h} = do
  withContext ctx $ do
    setFillStyle "black" ctx
    rect ctx { x: 0.0
             , y: 0.0
             , w: toNumber w
             , h: toNumber h
             }
    fill ctx


renderScene :: forall eff. Context2D -> Scene -> (Eff (canvas :: Canvas | eff) Unit)
renderScene ctx (Scene planets dimens) = do
  blackness ctx dimens
  renderPlanets ctx planets
  return unit

circle ctx c = withContext ctx $ do
  beginPath ctx
  setFillStyle c.color ctx
  arc ctx {x: c.x, y: c.y, r: c.r, start: 0.0, end: Math.pi * 2.0}
  fill ctx
  closePath ctx

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
      scene' <- scene (sampleOn frameRate spaces) dimens
      runSignal $ map (renderScene ctx) scene'
      runSignal $ map pressedSpace spaces
      runSignal $ map (setCanvasSize canvasElement) dimens
      -- runSignal spaceEffect
