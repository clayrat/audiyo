module Main where

import Prelude ( Unit, unit, bind, (>>=), return, ($), void
               , (<), (*), (-), otherwise )

import Control.Bind ((=<<))
import Control.Monad.Eff (Eff)

import DOM (DOM)
import Data.DOM.Simple.Window ( globalWindow, setInterval )

import Audio.WebAudio.Types ( WebAudio, OscillatorNode, AudioContext )
import Audio.WebAudio.AudioContext ( currentTime, destination, connect
                                   , createOscillator, makeAudioContext)
import Audio.WebAudio.AudioParam ( setValue )
import Audio.WebAudio.OscillatorNode ( OscillatorType(Sine), frequency
                                     , startOscillator, setOscillatorType)

main :: forall eff. (Eff (wau :: WebAudio, dom :: DOM | eff) Unit)
main = do
  ctx <- makeAudioContext

  osc <- createOscillator ctx
  setOscillatorType Sine osc
  startOscillator 0.0 osc

  connect osc =<< destination ctx

  play ctx osc

play :: forall eff. AudioContext -> OscillatorNode -> Eff (wau :: WebAudio, dom :: DOM | eff) Unit
play ctx osc = void $ setInterval globalWindow 10.0 update
  where
  update = do
    t <- currentTime ctx
    frequency osc >>= setValue (freqAt2 t)
    return unit

freqAt :: Number -> Number
freqAt t | t < 1.0 = 440.0
         | t < 2.0 = 880.0
         | otherwise = freqAt $ t - 2.0

freqAt2 :: Number -> Number
freqAt2 t | t <  4.50 = 164.81 * 2.0
          | t <  5.00 = 196.00 * 2.0
          | t <  5.75 = 130.81 * 2.0
          | t <  6.00 = 146.83 * 2.0
          | t <  8.00 = 164.81 * 2.0
          | t < 10.50 = 174.61 * 2.0
          | t < 12.00 = 164.81 * 2.0
          | t < 13.00 = 196.00 * 2.0
          | t < 13.50 = 174.61 * 2.0
          | t < 14.00 = 146.83 * 2.0
          | t < 16.00 = 130.81 * 2.0
          | otherwise = freqAt2 $ t - 16.0
