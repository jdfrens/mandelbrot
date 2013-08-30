{-
  No warranty.  No guarantees of any sort.
  Creative Commons Attribution-Share Alike 3.0 United States License
-}

module Fractals.Color (ColorMap(..), makeColorFunction)
where

import System.Random
import Data.List
import Data.Complex

import Fractals
import Fractals.PPM (ppmMaxColor, ppmEntry, ppmEntryGray)

data ColorMap = BlackOnWhite | WhiteOnBlack | Gray | Red | Green | Blue | Random
              deriving (Show, Eq)

makeColorFunction :: ColorMap -> Int -> FractalMembership a Integer -> String
makeColorFunction color seed =
  case color of
    BlackOnWhite -> blackOnWhite
    WhiteOnBlack -> whiteOnBlack
    Gray         -> grayScale
    Red          -> redScale
    Green        -> greenScale
    Blue         -> blueScale
    Random       -> randomColors $ randomColorsGenerator seed


white = ppmEntryGray ppmMaxColor
black = ppmEntryGray 0

-----------------------------------------------------------
-- black, white, and gray
-----------------------------------------------------------
blackOnWhite Inside = black
blackOnWhite (Outside _ _) = white

whiteOnBlack Inside = white
whiteOnBlack (Outside _ _) = black

grayScale Inside = black
grayScale (Outside _ n) = ppmEntryGray val
  where
    val = round $ fromInteger ppmMaxColor * (sqrt $ fromInteger n / fromInteger max_iters)^2


-----------------------------------------------------------
-- basic primary-color scales
-- coloring scheme from http://warp.povusers.org/Mandelbrot/
-----------------------------------------------------------
redScale   = povScale (\c -> ppmEntry c 0 0) (\c -> ppmEntry ppmMaxColor c c)
greenScale = povScale (\c -> ppmEntry 0 c 0) (\c -> ppmEntry c ppmMaxColor c)
blueScale  = povScale (\c -> ppmEntry 0 0 c) (\c -> ppmEntry c c ppmMaxColor)

povScale _ _ Inside = black
povScale plateau border (Outside _ n)
  | n < half_iters = plateau (scaleIter n)
  | otherwise      = border $ scaleIter $ n - half_iters
  where
    half_iters = (div max_iters 2) - 1
    scaleIter i = round $ iterRatio i * fromInteger ppmMaxColor
    iterRatio i = fromInteger (2*(i-1)) / fromInteger max_iters


-----------------------------------------------------------
-- random colors
-----------------------------------------------------------
randomColors _      Inside        = black
randomColors colors (Outside _ n) = genericIndex colors n

randomColorsGenerator seed = colorStream $ randomRs (0, ppmMaxColor) (mkStdGen seed)
  where
    colorStream (r : g : b : rest) = ppmEntry r g b : colorStream rest

-----------------------------------------------------------
-- helpers
-----------------------------------------------------------
smoother z n = fromInteger n + 1.0 - log (log  (magnitude z)) / log 2.0
