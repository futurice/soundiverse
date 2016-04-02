module Main where

import Prelude

import Data.Maybe
import Data.ArrayBuffer.Types (Uint8Array)

import Control.Timer
import Control.Monad.Eff
import Control.Monad.Eff.Random
import Control.Monad.Eff.Console

import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Canvas.Free

import Signal (Signal, runSignal, constant)
import Signal.DOM (animationFrame)
import Signal.Time (Time)

foreign import data AUDIO :: !

--main = do
--  canvas <- getCanvasElementById "canvas"
--  case canvas of
--    Nothing -> error "No canvas"
--    Just canvasElement -> do
--      context <- getContext2D canvasElement
--      runGraphics context $ do
--        -- Canvas API calls will go here
--        setFillStyle "#AAB333"
--        rect { x: 0.0, y: 0.0, w: 400.0, h: 400.0 }
--        fill

--foreign import randomArray :: forall eff. Eff (random :: RANDOM | eff) (Array Number)

--foreign import fillArray :: Int -> Int -> Array Int

-- foreign import constArray :: forall eff. Eff (console :: CONSOLE | eff) (Array Int)
-- foreign import onDOMContentLoaded :: forall a eff. Eff (dom :: DOM | eff) a -> Eff (eff) Unit
--
-- foreign import data WebAudio :: !
-- foreign import data AudioContext :: *
-- foreign import data AnalyserNode :: *
--
-- foreign import getAudioContext :: forall wau eff. Eff (wau :: WebAudio, console :: CONSOLE | eff) AudioContext
--
-- foreign import getAnalyserNode :: forall wau eff. AudioContext -> (Eff (wau :: WebAudio, console :: CONSOLE | eff) AnalyserNode)
--
-- foreign import getFFTSize
--   :: forall wau. AnalyserNode -> (Eff (wau :: WebAudio, console :: CONSOLE, dom :: DOM) Int)
--
-- foreign import getByteFrequencyData :: forall wau. AnalyserNode -> (Eff (wau :: WebAudio, console :: CONSOLE, dom :: DOM) (Array Int))

--constString :: String -> String
--constString a = a

--fillarray :: Int -> Array Int
--fillarray c = [c, c, c]

--constArray :: Array Int
--constArray = [1, 2]

-- type MicrophoneSignal = Eff (dom :: DOM, timer :: TIMER) (Signal (Array Int))
--
-- foreign import microphoneInputFrameP :: forall e c. (c -> Signal c) -> AudioContext -> Eff (wau :: WebAudio, dom :: DOM, timer :: TIMER | e) Time -> MicrophoneSignal
--
-- microphoneInputFrame :: forall wau e. AudioContext -> Eff (wau :: WebAudio, dom :: DOM, timer :: TIMER | e) Time -> MicrophoneSignal
-- microphoneInputFrame audioCtx = microphoneInputFrameP constant audioCtx
--
-- --type FrameSignal = Eff (dom :: DOM, timer :: TIMER) (Signal Time)
--
-- logshow :: forall e a. Show a => a -> Eff (console :: CONSOLE | e) Unit
-- logshow = log <<< show
--
-- maplogshow :: forall a e. Show a => Signal a -> Signal (Eff (console :: CONSOLE | e) Unit)
-- maplogshow = map logshow
--
--
-- --main :: forall t19. Eff ( timer :: TIMER, console :: CONSOLE | t19) Unit
-- main = onDOMContentLoaded do
-- 	ctx <- getAudioContext
-- 	-- analyser <- getAnalyserNode ctx
-- 	inputStream <- microphoneInputFrame ctx
-- 	runSignal $ maplogshow inputStream
-- 	-- runSignal (map log stringified)
--   	--frame <- animationFrame
--   	--runSignal (map log (map show frame))

foreign import audioStreamP :: forall e c. (c -> Signal c) -> Eff (audio :: AUDIO | e) (Signal String)

audioStream :: forall e. Eff (audio :: AUDIO | e) (Signal String)
audioStream = audioStreamP constant


main = do
  log "hey"
  audio <- audioStream
  runSignal $ map log audio
