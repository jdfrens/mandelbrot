{-# LANGUAGE OverloadedStrings #-}

module Options2Test where

import Test.HUnit
import Options2

decodedData input =
  case decodemf input of
    Just options  ->  options
    Nothing       ->  error "WTF?"

tests = let
  expected = (Options {fractal = Mandelbrot,
                       seed    = 12345,
                       upperLeftReal = 2.0
                      })
  input = concat ["---\n",
                  "fractal: Mandelbrot\n",
                  "seed: 12345\n",
                  "upperLeftReal: 2.0\n"]
             in
   TestList [
     expected ~=? (decodedData input)
     ]
