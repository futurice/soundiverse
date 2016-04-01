module Main where

import Prelude

import Data.Maybe

import Control.Monad.Eff
import Control.Monad.Eff.Console (error, log)

import Graphics.Canvas (getCanvasElementById, getContext2D)
import Graphics.Canvas.Free

import Signal
import Signal.DOM

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

main = do
  frame <- animationFrame
  runSignal (log <$> (show <$> frame))