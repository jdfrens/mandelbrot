{-# LANGUAGE OverloadedStrings #-}

module Options2Test where

import Test.HUnit
import Test.HUnit.Tools
import Data.Yaml as Y
import Data.Complex
import Control.Exception
import Control.Monad

import Fractals
import Options2

tests = TestList $ fractalTypeTests ++ optionsTests

instance Eq ParseException where
  (AesonException msg1) == (AesonException msg2)  =  msg1 == msg2
  _ == _  =  False

fractalTypeTests = [
  "parse FractalTypes" ~: (Right Mandelbrot) ~=? (decodemf "Mandelbrot"),
  "parse FractalTypes" ~: (Right Julia) ~=? (decodemf "Julia"),
  "parse FractalTypes" ~: (Right BurningShip) ~=? (decodemf "BurningShip"),
  "parse FractalTypes" ~: (Right Newton) ~=? (decodemf "Newton"),
  TestCase $ assertRaises "bad FractalTypes parse" (AesonException "poop is not a valid fractal type") (evaluate $ parseFractalType "poop")
  ]

optionsTests = [
  let
    message = "parses a full specification"
    expected = (Options {
                   fractal    = Mandelbrot,
                   color      = "bw",
                   size       = Dimension 720 480,
                   seed       = 12345,
                   upperLeft  =  0.0 :+  55.2,
                   lowerRight = 92.3 :+ 120.3,
                   c          = 3.14 :+ 4.13,
                   z          = 4.4  :+ 1.1,
                   r          = 9.9  :+ 3.3,
                   p          = 0.3  :+ 0.5
                   })
    input = concatMap (\s -> s ++ "\n")  [
      "fractal: Mandelbrot",
      "size: 720x480",
      "color: bw",  -- TODO: needs to be different
      "seed: 12345",
      "upperLeft: 0.0+55.2i",
      "lowerRight: 92.3+120.3i",
      "c: 3.14+4.13i",
      "z: 4.4+1.1i",
      "r: 9.9+3.3i",
      "p: 0.3+0.5i"]
  in
   testParse message expected input,

  let
    message = "parses a small specification"
    expected = Options {
      fractal    = Julia,
      size       = Dimension 512 384,
      color      = "bw",
      seed       = 666,
      upperLeft  = 5.0 :+ 6.0,
      lowerRight = 6.0 :+ 5.0,
      c          = 1.0 :+ 0.0,
      z          = 0.0 :+ 0.0,
      r          = 0.0 :+ 0.0,
      p          = 0.0 :+ 0.0
      }
    input = concatMap (\s -> s ++ "\n")  [
      "fractal: Julia",
      "upperLeft: 5.0+6.0i",
      "lowerRight: 6.0+5.0i"
      ]
  in
   testParse message expected input,

  let
    message = "parses a small specification from a file"
    expected = Options {
      fractal    = Mandelbrot,
      size       = Dimension 512 384,
      color      = "bw",
      seed       = 666,
      upperLeft  = (negate 2.0) :+ 1.2,
      lowerRight = 1.2 :+ (negate 1.2),
      c          = 1.0 :+ 0.0,
      z          = 0.0 :+ 0.0,
      r          = 0.0 :+ 0.0,
      p          = 0.0 :+ 0.0
      }
    filename = "../yaml/mandelbrot-black.yml"
  in
   TestLabel message $ TestCase $ do result <- decodeFractalFile filename
                                     case result of
                                       Left  exception -> assertFailure $ show exception
                                       Right config    -> assertEqual message expected config
   ]

testParse message expected input = message ~: expected ~=? (decodedData input)
  where decodedData input =
          case decodemf input of
            Left message  ->  error message
            Right config  ->  config
