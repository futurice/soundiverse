module Main where

import Prelude

import Data.Maybe
import Data.List
import Data.Int (toNumber)
import Data.Date (now, Now, toEpochMilliseconds)
import Data.Time
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
  , red :: Int
  , green :: Int
  , blue :: Int
  , timestamp :: Number
  }

maxAge = 5000.0

getTimestamp :: Milliseconds -> Number
getTimestamp (Milliseconds time) = time

randomPlanet :: forall e. DimensionPair -> Eff (random :: RANDOM, now :: Now | e) Planet
randomPlanet {w, h} = do
  r <- randomInt 0 256
  g <- randomInt 0 256
  b <- randomInt 0 256

  x <- randomInt 0 w
  y <- randomInt 0 h
  radius <- randomInt 0 100

  datetime <- now

  let timestamp = getTimestamp $ toEpochMilliseconds datetime


  return { x: toNumber x
         , y: toNumber y
         , r: toNumber radius
         , red: r
         , green: g
         , blue: b
         , timestamp: timestamp
         }

identity :: forall a. a -> a
identity x = x

frameRate :: Signal Number
frameRate = every 33.0

data Scene = Scene (List Planet) DimensionPair

scene :: forall eff. Signal Boolean -> Signal DimensionPair -> Eff (random :: RANDOM, now :: Now | eff) (Signal Scene)
scene spaces dimens = do
  planets <- unwrap $ map2 (\ds _ -> randomPlanet ds) dimens (filter identity false spaces)
  let planetList = foldp Cons Nil planets
  return $ map2 Scene planetList dimens

renderPlanets :: forall eff. Context2D -> Number -> List Planet -> (Eff (canvas :: Canvas | eff) Context2D)
renderPlanets ctx timestamp Nil = do
  return ctx
renderPlanets ctx timestamp (Cons planet scene) = do
  renderPlanet ctx timestamp planet
  renderPlanets ctx timestamp scene

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

renderScene :: forall eff. Context2D -> Scene -> (Eff (canvas :: Canvas, now :: Now | eff) Unit)
renderScene ctx (Scene planets dimens) = do
  datetime <- now
  let timestamp = getTimestamp $ toEpochMilliseconds datetime
  blackness ctx dimens
  renderPlanets ctx timestamp planets
  return unit

min a b = if a < b then a else b
max a b = if a > b then a else b

calculateOpacity nowstamp birthstamp =
  let age = nowstamp - birthstamp
  in 1.0 - ((max (min age maxAge) 1.0) / maxAge)

renderPlanet :: forall eff. Context2D -> Number -> Planet -> (Eff (canvas :: Canvas | eff) Context2D)
renderPlanet ctx timestamp planet = withContext ctx $ do
  let opacity = calculateOpacity timestamp planet.timestamp
  let color = "rgba(" ++ (show planet.red) ++ ", "
                      ++ (show planet.green) ++ ", "
                      ++ (show planet.blue) ++ ", "
                      ++ (show opacity) ++ ")"
  beginPath ctx
  setFillStyle color ctx
  arc ctx {x: planet.x, y: planet.y, r: planet.r, start: 0.0, end: Math.pi * 2.0}
  fill ctx
  closePath ctx

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
      runSignal $ map (setCanvasSize canvasElement) dimens
