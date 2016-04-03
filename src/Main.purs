module Main where

import Prelude ( Unit
               , map
               , ($)
               , bind
               , unit
               , return
               , (*)
               , (/)
               , (-)
               , (<)
               , otherwise
               , (>)
               , (+))

import Data.Maybe (Maybe(Just, Nothing))
import Data.List (List(Cons, Nil))
import Data.Int (toNumber)
import Data.Date (now, Now, toEpochMilliseconds)
import Data.Time (Milliseconds(Milliseconds))
import Data.Foldable (foldl)

import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, error)
import Control.Monad.Eff.Random (randomInt, randomRange, RANDOM)

import Color

import Graphics.Canvas ( Canvas
                       , CanvasElement
                       , Context2D
                       , getContext2D
                       , getCanvasElementById
                       , setCanvasHeight
                       , setCanvasWidth
                       , closePath
                       , fill
                       , arc
                       , setFillStyle
                       , beginPath
                       , withContext
                       , rect)

import Signal.DOM (windowDimensions, DimensionPair)
import Signal (runSignal, Signal, foldp, map2, unwrap, constant)
import Math (max, min, pi)
import DOM (DOM)

foreign import data AUDIO :: !

type Planet =
  { x :: Number
  , y :: Number
  , r :: Number
  , color :: Color
  , timestamp :: Number
  }

maxAge :: Number
maxAge = 3000.0

getTimestamp :: Milliseconds -> Number
getTimestamp (Milliseconds time) = time

timestamp' :: forall e. Eff (now :: Now | e) Number
timestamp' = do
  date <- now
  return $ getTimestamp $ toEpochMilliseconds date

randomPlanet :: forall e. Number -> DimensionPair -> Eff (random :: RANDOM, now :: Now | e) Planet
randomPlanet radius {w, h} = do
  hue <- randomRange 0.0 360.0
  let col = hsl hue 1.0 (1.0-(radius / 256.0))

  x <- randomInt 0 w
  y <- randomInt 0 h

  timestamp <- timestamp'

  return { x: toNumber x
         , y: toNumber y
         , r: radius
         , color: col
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

generatePlanet :: forall eff. DimensionPair -> Array Int -> Eff (random :: RANDOM, now :: Now | eff) Planet
generatePlanet dimens audio = randomPlanet (amplitude audio) dimens

data Scene = Scene (List Planet) DimensionPair

expired :: Number -> Planet -> Boolean
expired now planet = (now - planet.timestamp) < maxAge

scenes :: forall eff. Signal (Array Int) -> Signal DimensionPair -> Eff (random :: RANDOM, now :: Now | eff) (Signal Scene)
scenes audio dimens = do
  planets <- unwrap $ map2 generatePlanet dimens audio
  timestamp <- timestamp'

  let planetHasExpired = expired timestamp
      filterPlanetList = Data.List.filter planetHasExpired
      planetList = map filterPlanetList $ foldp Cons Nil planets
  return $ map2 Scene planetList dimens

renderPlanets :: forall eff. Context2D -> Number -> List Planet -> (Eff (canvas :: Canvas | eff) Context2D)
renderPlanets ctx timestamp Nil = do
  return ctx
renderPlanets ctx timestamp (Cons planet planets) = do
  renderPlanet ctx timestamp planet
  renderPlanets ctx timestamp planets

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

calculateOpacity :: Number -> Number -> Number
calculateOpacity nowstamp birthstamp =
  let age = nowstamp - birthstamp
  in 1.0 - ((max (min age maxAge) 1.0) / maxAge)

renderPlanet :: forall eff. Context2D -> Number -> Planet -> (Eff (canvas :: Canvas | eff) Context2D)
renderPlanet ctx timestamp planet = withContext ctx $ do
  let opacity = calculateOpacity timestamp planet.timestamp
      pColor = toRGBA planet.color
      renderColor = rgba pColor.r pColor.g pColor.b opacity
      color = cssStringRGBA renderColor
  beginPath ctx
  setFillStyle color ctx
  arc ctx {x: planet.x, y: planet.y, r: planet.r, start: 0.0, end: pi * 2.0}
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

main :: forall e. Eff (audio :: AUDIO, canvas :: Canvas, random :: RANDOM, now :: Now, dom :: DOM, console :: CONSOLE | e) Unit
main = do
  canvas <- getCanvasElementById "canvas"
  case canvas of
    Nothing -> error "No canvas"
    Just canvasElement -> do
      ctx <- getContext2D canvasElement
      dimens <- windowDimensions
      audios <- audioStream
      scenes' <- scenes audios dimens
      runSignal $ map (renderScene ctx) scenes'
      runSignal $ map (setCanvasSize canvasElement) dimens
