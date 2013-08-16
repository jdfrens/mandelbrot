{-# LANGUAGE OverloadedStrings #-}

module OptionsTest where

import Test.HUnit
import Test.HUnit.Tools
import Data.Yaml as Y
import Data.Complex
import Control.Exception
import Control.Monad

import Fractals
import Fractals.Color
import Fractals.Options

optionsModuleTests = TestList [fractalTypeTests, colorMapTests, optionsTests]

instance Eq ParseException where
  (AesonException msg1) == (AesonException msg2)  =  msg1 == msg2
  _ == _  =  False

fractalTypeTests = TestLabel "FractalType tests" $ TestList [
  "parse Mandelbrot"  ~: (Right Mandelbrot)  ~=? (decodemf "Mandelbrot"),
  "parse Julia"       ~: (Right Julia)       ~=? (decodemf "Julia"),
  "parse BurningShip" ~: (Right BurningShip) ~=? (decodemf "BurningShip"),
  "parse Newton"      ~: (Right Newton)      ~=? (decodemf "Newton"),
  let
    expectedException = AesonException "poop is not a valid fractal type"
    theParse          = evaluate $ parseFractalType "poop"
  in
   TestCase $ assertRaises "parse failure" expectedException theParse
  ]

colorMapTests = TestLabel "ColorMap tests" $ TestList [
  "parse BlackOnWhite" ~: (Right BlackOnWhite) ~=? (decodemf "BlackOnWhite"),
  "parse WhiteOnBlack" ~: (Right WhiteOnBlack) ~=? (decodemf "WhiteOnBlack"),
  "parse Red"    ~: (Right Red)    ~=? (decodemf "Red"),
  "parse Green"  ~: (Right Green)  ~=? (decodemf "Green"),
  "parse Blue"   ~: (Right Blue)   ~=? (decodemf "Blue"),
  "parse Random" ~: (Right Random) ~=? (decodemf "Random"),
  let
    expectedException = AesonException "poop is not a valid color map"
    theParse          = evaluate $ parseColorMap "poop"
  in
   TestCase $ assertRaises "parse failure" expectedException theParse
  ]

optionsTests = TestLabel "Options tests" $ TestList [
  let
    expected = Options {
      fractal    = Mandelbrot,
      color      = BlackOnWhite,
      size       = Dimension 720 480,
      seed       = 12345,
      upperLeft  =  0.0 :+  55.2,
      lowerRight = 92.3 :+ 120.3,
      c          = 3.14 :+ 4.13,
      z          = 4.4  :+ 1.1,
      r          = 9.9  :+ 3.3,
      p          = 0.3  :+ 0.5
      }
    input = yamlInput [
      "fractal: Mandelbrot",
      "size: 720x480",
      "color: BlackOnWhite",
      "seed: 12345",
      "upperLeft: 0.0+55.2i",
      "lowerRight: 92.3+120.3i",
      "c: 3.14+4.13i",
      "z: 4.4+1.1i",
      "r: 9.9+3.3i",
      "p: 0.3+0.5i"
      ]
  in
   testParse "parses a full specification" expected input,

  let
    expected = Options {
      fractal    = Julia,
      size       = Dimension 512 384,
      color      = BlackOnWhite,
      seed       = 666,
      upperLeft  = 5.0 :+ 6.0,
      lowerRight = 6.0 :+ 5.0,
      c          = 1.0 :+ 0.0,
      z          = 0.0 :+ 0.0,
      r          = 0.0 :+ 0.0,
      p          = 0.0 :+ 0.0
      }
    input = yamlInput [
      "fractal: Julia",
      "upperLeft: 5.0+6.0i",
      "lowerRight: 6.0+5.0i"
      ]
  in
   testParse "parses a small specification" expected input,

  let
    expected = Options {
      fractal    = Mandelbrot,
      size       = Dimension 512 384,
      color      = BlackOnWhite,
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
   testFileParse "parses a small specification from a file" expected filename
   ]

testParse message expected input = message ~: expected ~=? (decodedData input)
  where decodedData input =
          case decodemf input of
            Left message  ->  error message
            Right config  ->  config

testFileParse message expected filename = TestLabel message $ TestCase $ parse
   where parse = do result <- decodeFractalFile filename
                    case result of
                      Left  exception -> assertFailure $ show exception
                      Right config    -> assertEqual message expected config

yamlInput = concatMap (\s -> s ++ "\n")
