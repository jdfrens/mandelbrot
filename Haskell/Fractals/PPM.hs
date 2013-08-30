{-
  No warranty.  No guarantees of any sort.
  Creative Commons Attribution-Share Alike 3.0 United States License
-}

module Fractals.PPM (ppmPrefix, ppmMaxColor,
                     ppmEntry, ppmEntryGray)
where

import Fractals (Dimension(..))

ppmPrefix (Dimension width height) =
  ["P3", show width, show height, show ppmMaxColor]

ppmMaxColor = 255

ppmEntry :: Integer -> Integer -> Integer -> String
ppmEntry r g b = (show r) ++ " " ++ (show g) ++ " " ++ (show b)
ppmEntryGray n = ppmEntry n n n
