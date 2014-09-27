{-# LANGUAGE OverloadedStrings #-}

module OptionsTest where

import Control.Exception (evaluate)
import Data.Complex ( Complex(..) )

import Data.Yaml ( ParseException(AesonException) )
import Test.Framework ( testGroup )
import Test.Framework.Providers.HUnit ( testCase )
import Test.Framework.Providers.QuickCheck2 ( testProperty )
import Test.HUnit ( (@?=), assertFailure, assertEqual )
import Test.HUnit.Tools ( assertRaises )
import Test.QuickCheck (Arbitrary, Gen, arbitrary, choose, elements)

import Fractals
import Fractals.Color
import Fractals.Options

-- Top-level collections of all tests

optionsModuleTests = [
  testProperty "parsing complex numbers" prop_parsingComplex,
  testProperty "parsing fractal type" prop_parsingFractalType,
  testProperty "parsing dimensions" prop_parsingDimension,
  testProperty "parsing color map" prop_parsingColorMap,
  testProperty "parsing options yaml" prop_parsingOptions,
  testGroup "complex parse tests" parsingComplexTests,
  testGroup "FractalType tests"fractalTypeTests, 
  testGroup "ColorMap tests" colorMapTests, 
  testGroup "Options tests" optionsTests
  ]


-- HUnit tests

parsingComplexTests = [
  testCase "parse 0+0i"      $ Right (0.0 :+ 0.0)  @?= (decodemf "0+0i"),
  testCase "parse positives" $ Right (2.3 :+ 9.7)  @?= (decodemf "2.3+9.7i"),
  testCase "parse negatives" $ Right ((negate 1.5) :+ (negate 3.14))  @?= (decodemf "-1.5+-3.14i")
  ]

fractalTypeTests = [
  testCase "parse Mandelbrot"  $ (Right Mandelbrot)  @?= (decodemf "Mandelbrot"),
  testCase "parse Julia"       $ (Right Julia)       @?= (decodemf "Julia"),
  testCase "parse BurningShip" $ (Right BurningShip) @?= (decodemf "BurningShip"),
  testCase "parse Newton"      $ (Right Newton)      @?= (decodemf "Newton"),
  testCase "parse Nova"        $ (Right Nova)        @?= (decodemf "Nova"),
  let
    expectedException = AesonException "poop is not a valid fractal type"
    theParse          = evaluate $ parseFractalType "poop"
  in
   testCase "foobar" $ assertRaises "parse failure" expectedException theParse
  ]

colorMapTests =  [
  testCase "parse BlackOnWhite" $ (Right BlackOnWhite) @?= (decodemf "BlackOnWhite"),
  testCase "parse WhiteOnBlack" $ (Right WhiteOnBlack) @?= (decodemf "WhiteOnBlack"),
  testCase "parse Gray"   $ (Right Gray)   @?= (decodemf "Gray"),
  testCase "parse Red"    $ (Right Red)    @?= (decodemf "Red"),
  testCase "parse Green"  $ (Right Green)  @?= (decodemf "Green"),
  testCase "parse Blue"   $ (Right Blue)   @?= (decodemf "Blue"),
  testCase "parse Random" $ (Right Random) @?= (decodemf "Random"),
  let
    expectedException = AesonException "poop is not a valid color map"
    theParse          = evaluate $ parseColorMap "poop"
  in
   testCase "error on bad color map" $ assertRaises "parse failure" expectedException theParse
  ]

optionsTests = [
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

testParse message expected input = testCase message $ expected @?= (decodedData input)
  where decodedData input =
          case decodemf input of
            Left message  ->  error message
            Right config  ->  config

testFileParse message expected filename = testCase message $ parse
   where parse = do result <- decodeFractalFile filename
                    case result of
                      Left  exception -> assertFailure $ show exception
                      Right config    -> assertEqual message expected config

yamlInput = concatMap (\s -> s ++ "\n")


-- QuickCheck properties

prop_parsingComplex z = (decodemf (encodemf z)) == (Right z)
  where types = (z :: Complex Float)

prop_parsingFractalType ft = (decodemf (encodemf ft)) == (Right ft)
  where types = (ft :: FractalType)

prop_parsingDimension d = (decodemf (encodemf d)) == (Right d)
  where types = (d :: Dimension)

prop_parsingColorMap cm = (decodemf (encodemf cm)) == (Right cm)
  where types = (cm :: ColorMap)

prop_parsingOptions opts = (decodemf (encodemf opts)) == (Right opts)
  where types = (opts :: Options)


-- QuickCheck definitions

instance Arbitrary FractalType where
  arbitrary = elements [Mandelbrot, Julia, BurningShip, Newton, Nova]

instance Arbitrary Dimension where
  arbitrary = do
    w <- choose (1, 100000) :: Gen Integer
    h <- choose (1, 100000) :: Gen Integer
    return $ Dimension w h

instance Arbitrary ColorMap where
  arbitrary = elements [BlackOnWhite, WhiteOnBlack, Gray, Red, Green, Blue, Random]

instance Arbitrary Options where
  arbitrary = do
    fractal    <- arbitrary
    size       <- arbitrary
    color      <- arbitrary
    seed       <- arbitrary
    upperLeft  <- arbitrary
    lowerRight <- arbitrary
    c          <- arbitrary
    z          <- arbitrary
    r          <- arbitrary
    p          <- arbitrary
    return Options {
      fractal    = fractal   ,
      size       = size      ,
      color      = color     ,
      seed       = seed      ,
      upperLeft  = upperLeft ,
      lowerRight = lowerRight,
      c          = c         ,
      z          = z         ,
      r          = r         ,
      p          = p         
      }


-- Eq definitions for assertions

instance Eq ParseException where
  (AesonException msg1) == (AesonException msg2)  =  msg1 == msg2
  _ == _  =  False
