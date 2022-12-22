{-# LANGUAGE TemplateHaskell #-}
module Sound.Tidal.Params where

{-
    Params.hs - Provides the basic control patterns available to TidalCycles by default
    Copyright (C) 2021, Alex McLean and contributors

    This library is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this library.  If not, see <http://www.gnu.org/licenses/>.
-}

import qualified Data.Map.Strict as Map

import Sound.Tidal.ParamTemplates
import Sound.Tidal.Pattern
import Sound.Tidal.Core ((#))
import Sound.Tidal.Utils
import Data.Maybe (fromMaybe)
import Data.Word (Word8)
import Data.Fixed (mod')

-- | group multiple params into one
grp :: [String -> ValueMap] -> Pattern String -> ControlPattern
grp [] _ = empty
grp fs p = splitby <$> p
  where splitby name = Map.unions $ map (\(v, f) -> f v) $ zip (split name) fs
        split :: String -> [String]
        split = wordsBy (==':')

mF :: String -> String -> ValueMap
mF name v = fromMaybe Map.empty $ do f <- readMaybe v
                                     return $ Map.singleton name (VF f)

mI :: String -> String -> ValueMap
mI name v = fromMaybe Map.empty $ do i <- readMaybe v
                                     return $ Map.singleton name (VI i)

mS :: String -> String -> ValueMap
mS name v = Map.singleton name (VS v)

-- | Param makers

param :: Valuable a => String -> Pattern a -> ControlPattern
param name = fmap (Map.singleton name . toValue)

pF :: String -> Pattern Double -> ControlPattern
pF = param

pI :: String -> Pattern Int -> ControlPattern
pI = param

pB :: String -> Pattern Bool -> ControlPattern
pB = param
 
pR :: String -> Pattern Rational -> ControlPattern
pR = param

pN :: String -> Pattern Note -> ControlPattern
pN = param

pS :: String -> Pattern String -> ControlPattern
pS = param

pX :: String -> Pattern [Word8] -> ControlPattern
pX = param

pStateF ::
  String -> -- ^ A parameter, e.g. `note`; a
  -- `String` recognizable by a `ValueMap`.
  String -> -- ^ Identifies the cycling state pattern.
  -- Can be anything the user wants.
  (Maybe Double -> Double) ->
  ControlPattern
pStateF name sName update =
  pure $ Map.singleton name $ VState statef
  where statef :: ValueMap -> (ValueMap, Value)
        statef sMap = (Map.insert sName v sMap, v)
          where v = VF $ update
                    $ Map.lookup sName sMap >>= getF

-- | `pStateList` is made with cyclic lists in mind,
-- but it can even "cycle" through infinite lists.
pStateList ::
  Valuable a =>
  String -> -- ^ A parameter, e.g. `note`; a
  -- `String` recognizable by a `ValueMap`.
  String -> -- ^ Identifies the cycling state pattern.
  -- Can be anything the user wants.
  [a] -> -- ^ The list to cycle through.
  ControlPattern
pStateList name sName xs =
  pure $ Map.singleton name $ VState statef
  where
    statef :: ValueMap -> (ValueMap, Value)
    statef sMap = ( Map.insert sName
                    (VList $ tail looped) sMap
                  , head looped)
      where xs' = fromMaybe (map toValue xs)
                  $ Map.lookup sName sMap >>= getList
            -- do this instead of a cycle, so it can get updated with the a list
            looped | null xs' = map toValue xs
                   | otherwise = xs'

-- | A wrapper for `pStateList` that accepts a `[Double]`
-- rather than a `[Value]`.
pStateListF :: String -> String -> [Double] -> ControlPattern
pStateListF name sName = pStateList name sName

-- | A wrapper for `pStateList` that accepts a `[String]`
-- rather than a `[Value]`.
pStateListS :: String -> String -> [String] -> ControlPattern
pStateListS name sName = pStateList name sName

-- | Grouped params

sound :: Pattern String -> ControlPattern
sound = grp [mS "s", mF "n"]

$(alias "sound" "s")

sTake :: String -> [String] -> ControlPattern
sTake name xs = pStateListS "s" name xs

cc :: Pattern String -> ControlPattern
cc = grp [mF "ccn", mF "ccv"]

nrpn :: Pattern String -> ControlPattern
nrpn = grp [mI "nrpn", mI "val"]

nrpnn :: Pattern Int -> ControlPattern
nrpnn = pI "nrpn"

nrpnv :: Pattern Int -> ControlPattern
nrpnv = pI "val"

grain' :: Pattern String -> ControlPattern
grain' = grp [mF "begin", mF "end"]

-- N/Note params

-- | The note or sample number to choose for a synth or sampleset
$(mkParam ''Note "n" [NoBus, Alias "number"])

-- | The note or pitch to play a sound or synth with
$(mkParam ''Note "note" [NoBus, Alias "up"])

midinote :: Pattern Note -> ControlPattern
midinote = note . (subtract 60 <$>)

drum :: Pattern String -> ControlPattern
drum = n . (subtract 60 . drumN <$>)

drumN :: Num a => String -> a
drumN "hq" = 27
drumN "sl" = 28
drumN "ps" = 29
drumN "pl" = 30
drumN "st" = 31
drumN "sq" = 32
drumN "ml" = 33
drumN "mb" = 34
drumN "ab" = 35
drumN "bd" = 36
drumN "rm" = 37
drumN "sn" = 38
drumN "cp" = 39
drumN "es" = 40
drumN "lf" = 41
drumN "ch" = 42
drumN "lt" = 43
drumN "hh" = 44
drumN "ft" = 45
drumN "oh" = 46
drumN "mt" = 47
drumN "hm" = 48
drumN "cr" = 49
drumN "ht" = 50
drumN "ri" = 51
drumN "cy" = 52
drumN "be" = 53
drumN "ta" = 54
drumN "sc" = 55
drumN "cb" = 56
drumN "cs" = 57
drumN "vi" = 58
drumN "rc" = 59
drumN "hb" = 60
drumN "lb" = 61
drumN "mh" = 62
drumN "hc" = 63
drumN "lc" = 64
drumN "he" = 65
drumN "le" = 66
drumN "ag" = 67
drumN "la" = 68
drumN "ca" = 69
drumN "ma" = 70
drumN "sw" = 71
drumN "lw" = 72
drumN "sg" = 73
drumN "lg" = 74
drumN "cl" = 75
drumN "hi" = 76
drumN "li" = 77
drumN "mc" = 78
drumN "oc" = 79
drumN "tr" = 80
drumN "ot" = 81
drumN "sh" = 82
drumN "jb" = 83
drumN "bt" = 84
drumN "ct" = 85
drumN "ms" = 86
drumN "os" = 87
drumN _ = 0
-- | a pattern of numbers that speed up (or slow down) samples while they play.
$(mkParamF "accelerate" [NoBus])

-- | like @gain@, but linear.
$(mkParamF "amp" [])

-- | 
$(mkParamX "array" [NoBus])

-- | a pattern of numbers to specify the attack time (in seconds) of an envelope applied to each sample.
$(mkParamF "attack" [Alias "att"])

-- | a pattern of numbers from 0 to 1. Sets the center frequency of the band-pass filter.
$(mkParamF "bandf" [Alias "bpf"])

-- | a pattern of anumbers from 0 to 1. Sets the q-factor of the band-pass filter.
$(mkParamF "bandq" [Alias "bpq"])

-- | a pattern of numbers from 0 to 1. Skips the beginning of each sample, e.g. `0.25` to cut off the first quarter from each sample.
$(mkParamF "begin" [NoBus])

-- | Spectral binshift
$(mkParamF "binshift" [])

-- | 
$(mkParamF "button0" [])

-- | 
$(mkParamF "button1" [])

-- | 
$(mkParamF "button10" [])

-- | 
$(mkParamF "button11" [])

-- | 
$(mkParamF "button12" [])

-- | 
$(mkParamF "button13" [])

-- | 
$(mkParamF "button14" [])

-- | 
$(mkParamF "button15" [])

-- | 
$(mkParamF "button2" [])

-- | 
$(mkParamF "button3" [])

-- | 
$(mkParamF "button4" [])

-- | 
$(mkParamF "button5" [])

-- | 
$(mkParamF "button6" [])

-- | 
$(mkParamF "button7" [])

-- | 
$(mkParamF "button8" [])

-- | 
$(mkParamF "button9" [])

-- | 
$(mkParamF "ccn" [NoBus])

-- | 
$(mkParamF "ccv" [NoBus])

-- | choose the channel the pattern is sent to in superdirt
$(mkParamI "channel" [NoBus])

-- | 
$(mkParamF "clhatdecay" [Alias "chdecay"])

-- | fake-resampling, a pattern of numbers for lowering the sample rate, i.e. 1 for original 2 for half, 3 for a third and so on.
$(mkParamF "coarse" [])

-- | Spectral comb
$(mkParamF "comb" [])

-- | 
$(mkParamF "control" [NoBus])

-- | 
$(mkParamF "cps" [])

-- | bit crushing, a pattern of numbers from 1 (for drastic reduction in bit-depth) to 16 (for barely no reduction).
$(mkParamF "crush" [])

-- | 
$(mkParamF "ctlNum" [NoBus])

-- | 
$(mkParamF "ctranspose" [])

-- | In the style of classic drum-machines, `cut` will stop a playing sample as soon as another samples with in same cutgroup is to be played. An example would be an open hi-hat followed by a closed one, essentially muting the open.
$(mkParamI "cut" [])

-- | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the low-pass filter.
$(mkParamF "cutoff" [Alias "ctf", Alias "lpf"])

-- | 
$(mkParamF "cutoffegint" [Alias "ctfg"])

-- | 
$(mkParamF "decay" [])

-- | 
$(mkParamF "degree" [])

-- | a pattern of numbers from 0 to 1. Sets the level of the delay signal.
$(mkParamF "delay" [])

-- | a pattern of numbers from 0 to 1. Sets the amount of delay feedback.
$(mkParamF "delayfeedback" [Alias "delayfb", Alias "dfb"])

-- | a pattern of numbers from 0 to 1. Sets the length of the delay.
$(mkParamF "delaytime" [Alias "delayt", Alias "dt"])

-- | 
$(mkParamF "detune" [Alias "det"])

-- | noisy fuzzy distortion
$(mkParamF "distort" [])

-- | DJ filter, below 0.5 is low pass filter, above is high pass filter.
$(mkParamF "djf" [])

-- | when set to `1` will disable all reverb for this pattern. See `room` and `size` for more information about reverb.
$(mkParamF "dry" [])

-- | 
$(mkParamF "dur" [])

-- | the same as `begin`, but cuts the end off samples, shortening them; e.g. `0.75` to cut off the last quarter of each sample.
$(mkParamF "end" [NoBus])

-- | Spectral enhance
$(mkParamF "enhance" [])

-- | 
$(mkParamF "expression" [])

-- | As with fadeTime, but controls the fade in time of the grain envelope. Not used if the grain begins at position 0 in the sample.
$(mkParamF "fadeInTime" [NoBus])

-- | Used when using begin/end or chop/striate and friends, to change the fade out time of the 'grain' envelope.
$(mkParamF "fadeTime" [NoBus, Alias "fadeOutTime"])

-- | 
$(mkParamF "frameRate" [NoBus])

-- | 
$(mkParamF "frames" [NoBus])

-- | Spectral freeze
$(mkParamF "freeze" [])

-- | 
$(mkParamF "freq" [])

-- | for internal sound routing
$(mkParamF "from" [])

-- | frequency shifter
$(mkParamF "fshift" [])

-- | frequency shifter
$(mkParamF "fshiftnote" [])

-- | frequency shifter
$(mkParamF "fshiftphase" [])

-- | a pattern of numbers that specify volume. Values less than 1 make the sound quieter. Values greater than 1 make the sound louder. For the linear equivalent, see @amp@.
$(mkParamF "gain" [NoBus])

-- | 
$(mkParamF "gate" [Alias "gat"])

-- | 
$(mkParamF "harmonic" [])

-- | 
$(mkParamF "hatgrain" [Alias "hg"])

-- | High pass sort of spectral filter
$(mkParamF "hbrick" [])

-- | a pattern of numbers from 0 to 1. Applies the cutoff frequency of the high-pass filter. Also has alias @hpf@
$(mkParamF "hcutoff" [Alias "hpf"])

-- | a pattern of numbers to specify the hold time (in seconds) of an envelope applied to each sample. Only takes effect if `attack` and `release` are also specified.
$(mkParamF "hold" [])

-- | 
$(mkParamF "hours" [NoBus])

-- | a pattern of numbers from 0 to 1. Applies the resonance of the high-pass filter. Has alias @hpq@
$(mkParamF "hresonance" [Alias "hpq"])

-- | 
$(mkParamF "imag" [])

-- | 
$(mkParamF "kcutoff" [])

-- | shape/bass enhancer
$(mkParamF "krush" [])

-- | 
$(mkParamF "lagogo" [Alias "lag"])

-- | Low pass sort of spectral filter
$(mkParamF "lbrick" [])

-- | 
$(mkParamF "lclap" [Alias "lcp"])

-- | 
$(mkParamF "lclaves" [Alias "lcl"])

-- | 
$(mkParamF "lclhat" [Alias "lch"])

-- | 
$(mkParamF "lcrash" [Alias "lcr"])

-- | controls the amount of overlap between two adjacent sounds
$(mkParamF "legato" [NoBus])

-- | 
$(mkParamF "leslie" [])

-- | 
$(mkParamF "lfo" [])

-- | 
$(mkParamF "lfocutoffint" [Alias "lfoc"])

-- | 
$(mkParamF "lfodelay" [])

-- | 
$(mkParamF "lfoint" [Alias "lfoi"])

-- | 
$(mkParamF "lfopitchint" [Alias "lfop"])

-- | 
$(mkParamF "lfoshape" [])

-- | 
$(mkParamF "lfosync" [])

-- | 
$(mkParamF "lhitom" [Alias "lht"])

-- | 
$(mkParamF "lkick" [Alias "lbd"])

-- | 
$(mkParamF "llotom" [Alias "llt"])

-- | A pattern of numbers. Specifies whether delaytime is calculated relative to cps. When set to 1, delaytime is a direct multiple of a cycle.
$(mkParamF "lock" [])

-- | loops the sample (from `begin` to `end`) the specified number of times.
$(mkParamF "loop" [NoBus])

-- | 
$(mkParamF "lophat" [Alias "loh"])

-- | 
$(mkParamF "lrate" [])

-- | 
$(mkParamF "lsize" [])

-- | 
$(mkParamF "lsnare" [Alias "lsn"])

-- | 
$(mkParamF "midibend" [NoBus])

-- | 
$(mkParamF "midichan" [NoBus])

-- | 
$(mkParamS "midicmd" [NoBus])

-- | 
$(mkParamF "miditouch" [NoBus])

-- | 
$(mkParamF "minutes" [NoBus])

-- | 
$(mkParamF "modwheel" [])

-- | 
$(mkParamF "mtranspose" [])

-- | Nudges events into the future by the specified number of seconds. Negative numbers work up to a point as well (due to internal latency)
$(mkParamF "nudge" [])

-- | 
$(mkParamI "octave" [NoBus])

-- | 
$(mkParamF "octaveR" [])

-- | octaver effect
$(mkParamF "octer" [])

-- | octaver effect
$(mkParamF "octersub" [])

-- | octaver effect
$(mkParamF "octersubsub" [])

-- | 
$(mkParamF "offset" [NoBus])

-- | 
$(mkParamF "ophatdecay" [Alias "ohdecay"])

-- | a pattern of numbers. An `orbit` is a global parameter context for patterns. Patterns with the same orbit will share hardware output bus offset and global effects, e.g. reverb and delay. The maximum number of orbits is specified in the superdirt startup, numbers higher than maximum will wrap around.
$(mkParamI "orbit" [])

-- | 
$(mkParamF "overgain" [NoBus])

-- | 
$(mkParamF "overshape" [])

-- | a pattern of numbers between 0 and 1, from left to right (assuming stereo), once round a circle (assuming multichannel)
$(mkParamF "pan" [])

-- | a pattern of numbers between -1.0 and 1.0, which controls the relative position of the centre pan in a pair of adjacent speakers (multichannel only)
$(mkParamF "panorient" [])

-- | a pattern of numbers between -inf and inf, which controls how much multichannel output is fanned out (negative is backwards ordering)
$(mkParamF "panspan" [])

-- | a pattern of numbers between 0.0 and 1.0, which controls the multichannel spread range (multichannel only)
$(mkParamF "pansplay" [])

-- | a pattern of numbers between 0.0 and inf, which controls how much each channel is distributed over neighbours (multichannel only)
$(mkParamF "panwidth" [])

-- | 
$(mkParamF "partials" [])

-- | Phaser Audio DSP effect | params are 'phaserrate' and 'phaserdepth'
$(mkParamF "phaserdepth" [Alias "phasdp"])

-- | Phaser Audio DSP effect | params are 'phaserrate' and 'phaserdepth'
$(mkParamF "phaserrate" [Alias "phasr"])

-- | 
$(mkParamF "pitch1" [Alias "pit1"])

-- | 
$(mkParamF "pitch2" [Alias "pit2"])

-- | 
$(mkParamF "pitch3" [Alias "pit3"])

-- | 
$(mkParamF "polyTouch" [NoBus])

-- | 
$(mkParamF "portamento" [Alias "por"])

-- | 
$(mkParamF "progNum" [NoBus])

-- | used in SuperDirt softsynths as a control rate or 'speed'
$(mkParamF "rate" [])

-- | Spectral conform
$(mkParamF "real" [])

-- | a pattern of numbers to specify the release time (in seconds) of an envelope applied to each sample.
$(mkParamF "release" [Alias "rel"])

-- | a pattern of numbers from 0 to 1. Specifies the resonance of the low-pass filter.
$(mkParamF "resonance" [Alias "lpq"])

-- | ring modulation
$(mkParamF "ring" [])

-- | ring modulation
$(mkParamF "ringdf" [])

-- | ring modulation
$(mkParamF "ringf" [])

-- | a pattern of numbers from 0 to 1. Sets the level of reverb.
$(mkParamF "room" [])

-- | 
$(mkParamF "sagogo" [Alias "sag"])

-- | 
$(mkParamF "sclap" [Alias "scp"])

-- | 
$(mkParamF "sclaves" [Alias "scl"])

-- | Spectral scramble
$(mkParamF "scram" [])

-- | 
$(mkParamF "scrash" [Alias "scr"])

-- | 
$(mkParamF "seconds" [NoBus])

-- | 
$(mkParamF "semitone" [])

-- | wave shaping distortion, a pattern of numbers from 0 for no distortion up to 1 for loads of distortion.
$(mkParamF "shape" [])

-- | a pattern of numbers from 0 to 1. Sets the perceptual size (reverb time) of the `room` to be used in reverb.
$(mkParamF "size" [Alias "sz"])

-- | 
$(mkParamF "slide" [Alias "sld"])

-- | 
$(mkParamF "slider0" [])

-- | 
$(mkParamF "slider1" [])

-- | 
$(mkParamF "slider10" [])

-- | 
$(mkParamF "slider11" [])

-- | 
$(mkParamF "slider12" [])

-- | 
$(mkParamF "slider13" [])

-- | 
$(mkParamF "slider14" [])

-- | 
$(mkParamF "slider15" [])

-- | 
$(mkParamF "slider2" [])

-- | 
$(mkParamF "slider3" [])

-- | 
$(mkParamF "slider4" [])

-- | 
$(mkParamF "slider5" [])

-- | 
$(mkParamF "slider6" [])

-- | 
$(mkParamF "slider7" [])

-- | 
$(mkParamF "slider8" [])

-- | 
$(mkParamF "slider9" [])

-- | Spectral smear
$(mkParamF "smear" [])

-- | 
$(mkParamF "songPtr" [NoBus])

-- | a pattern of numbers which changes the speed of sample playback, i.e. a cheap way of changing pitch. Negative values will play the sample backwards!
$(mkParamF "speed" [NoBus])

-- | 
$(mkParamF "squiz" [])

-- | 
$(mkParamF "stepsPerOctave" [])

-- | 
$(mkParamF "stutterdepth" [Alias "std"])

-- | 
$(mkParamF "stuttertime" [Alias "stt"])

-- | 
$(mkParamF "sustain" [NoBus, Alias "sus"])

-- | 
$(mkParamF "sustainpedal" [])

-- | time stretch amount
$(mkParamF "timescale" [NoBus])

-- | time stretch window size
$(mkParamF "timescalewin" [NoBus])

-- | for internal sound routing
$(mkParamF "to" [])

-- | for internal sound routing
$(mkParamS "toArg" [])

-- | 
$(mkParamF "tomdecay" [Alias "tdecay"])

-- | Tremolo Audio DSP effect | params are 'tremolorate' and 'tremolodepth'
$(mkParamF "tremolodepth" [Alias "tremdp"])

-- | Tremolo Audio DSP effect | params are 'tremolorate' and 'tremolodepth'
$(mkParamF "tremolorate" [Alias "tremr"])

-- | tube distortion
$(mkParamF "triode" [])

-- | 
$(mkParamF "tsdelay" [])

-- | 
$(mkParamF "uid" [NoBus])

-- | used in conjunction with `speed`, accepts values of "r" (rate, default behavior), "c" (cycles), or "s" (seconds). Using `unit "c"` means `speed` will be interpreted in units of cycles, e.g. `speed "1"` means samples will be stretched to fill a cycle. Using `unit "s"` means the playback speed will be adjusted so that the duration is the number of seconds specified by `speed`.
$(mkParamS "unit" [NoBus])

-- | 
$(mkParamF "val" [NoBus])

-- | 
$(mkParamF "vcfegint" [Alias "vcf"])

-- | 
$(mkParamF "vcoegint" [Alias "vco"])

-- | 
$(mkParamF "velocity" [])

-- | 
$(mkParamF "voice" [Alias "voi"])

-- | formant filter to make things sound like vowels, a pattern of either `a`, `e`, `i`, `o` or `u`. Use a rest (`~`) for no effect.
$(mkParamS "vowel" [])

-- | 
$(mkParamF "waveloss" [])

-- | 
$(mkParamF "xsdelay" [])
