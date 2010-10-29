{- 
  For GRHUG
  Version: July 2009
  Creative Commons Attribution-Share Alike 3.0 United States License
-}

module PPM (ppmPrefix, 
            blackOnWhite, whiteOnBlack, 
            grayScale, redScale, greenScale, blueScale,
            randomColors, randomColorsGenerator)
where
  
import System.Random
import Data.List
import Complex

import Fractals

max_color = 255
  
ppmPrefix (Dimension width height) =
  ["P3", show width, show height, show max_color]

ppmEntry :: Integer -> Integer -> Integer -> String
ppmEntry r g b = (show r) ++ " " ++ (show g) ++ " " ++ (show b)
ppmEntryGray n = ppmEntry n n n

white = ppmEntryGray max_color
black = ppmEntryGray 0

-----------------------------------------------------------
-- helpers
-----------------------------------------------------------
smoother z n = fromInteger n + 1.0 - log (log  (magnitude z)) / log 2.0


-----------------------------------------------------------
-- black, white, and gray
-----------------------------------------------------------
blackOnWhite Inside = black
blackOnWhite (Outside _ _) = white

whiteOnBlack Inside = white
whiteOnBlack (Outside _ _) = black

grayScale Inside = black
grayScale (Outside z n) = ppmEntryGray val
  where
    val = round $ fromInteger max_color * (sqrt $ fromInteger n / fromInteger max_iters)^2


-----------------------------------------------------------
-- basic primary-color scales
-- coloring scheme from http://warp.povusers.org/Mandelbrot/
-----------------------------------------------------------
redScale   = povScale (\c -> ppmEntry c 0 0) (\c -> ppmEntry max_color c c)
greenScale = povScale (\c -> ppmEntry 0 c 0) (\c -> ppmEntry c max_color c)
blueScale  = povScale (\c -> ppmEntry 0 0 c) (\c -> ppmEntry c c max_color)

povScale _ _ Inside = black
povScale plateau border (Outside _ n)
  | n < half_iters = plateau (scaleIter n)
  | otherwise      = border $ scaleIter $ n - half_iters
  where
    half_iters = (div max_iters 2) - 1
    scaleIter i = round $ iterRatio i * fromInteger max_color
    iterRatio i = fromInteger (2*(i-1)) / fromInteger max_iters


-----------------------------------------------------------
-- random colors
-----------------------------------------------------------
randomColors _      Inside        = black  
randomColors colors (Outside _ n) = genericIndex colors n
    
randomColorsGenerator seed = colorStream $ randomRs (0, max_color) (mkStdGen seed)
  where
    colorStream (r : g : b : rest) = ppmEntry r g b : colorStream rest
