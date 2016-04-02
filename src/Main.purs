module Main where

import Prelude

import Data.Array
import Data.Maybe
import Data.List
import Data.Int (toNumber, floor)
import Data.Date (now, Now, toEpochMilliseconds)
import Data.Time
import Debug.Trace (spy)
import Data.Foldable (foldl)

import Control.Apply (lift2)
import Control.Monad.Eff
import Control.Monad.Eff.Console (error, log, CONSOLE)
import Control.Monad.Eff.Random (randomInt, randomRange, RANDOM)

import Graphics.Canvas

import Signal.DOM (keyPressed, windowDimensions, DimensionPair)
import Signal (filter, runSignal, Signal, foldp, sampleOn, map2, unwrap, constant)
import Signal.Time (every)
import Math hiding (floor)

foreign import data AUDIO :: !

type Planet =
  { x :: Number
  , y :: Number
  , r :: Number
  , red :: Int
  , green :: Int
  , blue :: Int
  , timestamp :: Number
  }

maxAge = 2000.0

getTimestamp :: Milliseconds -> Number
getTimestamp (Milliseconds time) = time

timestamp' :: forall e. Eff (now :: Now | e) Number
timestamp' = do
  date <- now
  return $ getTimestamp $ toEpochMilliseconds date

freq2colorLowerBound :: forall e. Number -> Int
freq2colorLowerBound freq = floor (((min freq 2000.0) / 2000.0) * 255.0)

randomPlanet :: forall e. Number -> Number -> Number -> Number -> DimensionPair -> Eff (random :: RANDOM, now :: Now | e) Planet
randomPlanet r g b radius {w, h} = do

  x <- randomInt 0 w
  y <- randomInt 0 h

  timestamp <- timestamp'

  return { x: toNumber x
         , y: toNumber y
         , r: radius
         , red: floor r
         , green: floor g
         , blue: floor b
         , timestamp: timestamp
         }

sum :: Array Int -> Int
sum ints = foldl (+) 0 ints

len :: forall a. Array a -> Int
len = Data.Array.length

amplitude :: Array Int -> Number
amplitude ints
  | len ints > 0 = (toNumber (sum ints)) / (toNumber (len ints))
  | otherwise = 0.0

type PeakCounterState = { prevWasPositive :: Boolean
                        , count :: Number
                        }

doCount :: PeakCounterState -> Int -> PeakCounterState
doCount state next
  | state.prevWasPositive && next > 0 = state
  | state.prevWasPositive && next <= 0 = {prevWasPositive: false, count: state.count}
  | state.prevWasPositive == false && next > 0 = {prevWasPositive: true, count: state.count + 1.0}
  | state.prevWasPositive == false && next <= 0 = state

cutA = 200
cutB = 400

generatePlanet dimens audio =
  let r = amplitude $ Data.Array.slice 0 cutA audio
      g = amplitude $ Data.Array.slice cutA cutB audio
      b = amplitude $ Data.Array.slice cutB 2048 audio
  in randomPlanet r g b (amplitude audio) dimens

identity :: forall a. a -> a
identity x = x

frameRate :: Signal Number
frameRate = every 33.0

data Scene = Scene (List Planet) DimensionPair

expired :: Number -> Planet -> Boolean
expired now planet = (now - planet.timestamp) < maxAge

scene :: forall eff. Signal (Array Int) -> Signal DimensionPair -> Eff (random :: RANDOM, now :: Now | eff) (Signal Scene)
scene audio dimens = do
  planets <- unwrap $ map2 generatePlanet dimens audio
  timestamp <- timestamp'

  let planetHasExpired = expired timestamp
      filterPlanetList = Data.List.filter planetHasExpired
      planetList = map filterPlanetList $ foldp Cons Nil planets
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

--min a b = if a < b then a else b
--max a b = if a > b then a else b

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


foreign import audioStreamP :: forall e c. (c -> Signal c) -> Eff (audio :: AUDIO | e) (Signal (Array Int))

audioStream :: forall e. Eff (audio :: AUDIO | e) (Signal (Array Int))
audioStream = audioStreamP constant

main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> error "No canvas"
    Just canvasElement -> do
      ctx <- getContext2D canvasElement
      dimens <- windowDimensions
      audios <- audioStream
      scene' <- scene audios dimens
      runSignal $ map (renderScene ctx) scene'
      runSignal $ map (setCanvasSize canvasElement) dimens
