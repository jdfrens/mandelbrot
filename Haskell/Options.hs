{-
  For GRHUG
  Version: July 2009
  Creative Commons Attribution-Share Alike 3.0 United States License
-}

module Options
where

import System.Console.GetOpt
import Complex
import Text.Regex

import PPM
import Fractals
                       
data FractalType = Mandelbrot | Julia | BurningShip | Newton                                                

data Options a = Options { 
    optFractal  :: FractalType
  , optSize     :: Dimension Integer
  , optColor    :: SetMembership (Complex a) Integer -> String
  , optSeed     :: Int
  , optUpperLeft  :: Complex a
  , optLowerRight :: Complex a
  , optC          :: Complex a
  , optZ          :: Complex a
  , optR          :: Complex a
  , optP          :: Complex a
  }
defaultOptions = Options  { optFractal = Mandelbrot
                        , optSize    = Dimension 512 384
                        , optColor   = blackOnWhite
                        , optSeed    = 666
                        , optUpperLeft  = (negate 2.0) :+ 1.2
                        , optLowerRight = 1.2 :+ (negate 1.2)
                        , optC = 1.0 :+ 0.0
                        , optZ = 0.0 :+ 0.0
                        , optR = 0.0 :+ 0.0
                        , optP = 0.0 :+ 0.0
                        }

options =
  [ Option "t" ["type"] (ReqArg typeFunc "TYPE") "type of fractal"
  , Option "s" ["size"] (ReqArg sizeFunc "WIDTHxHEIGHT") "size of image"
  , Option ""  ["color"] (ReqArg colorFunc "COLOR") "color map"
  , Option ""  ["seed"]  (ReqArg seedFunc "SEED") "random seed"
  , Option ""  ["upperleft"] (ReqArg upperLeftFunc "UPPERLEFT") "upper left corner"
  , Option ""  ["lowerright"] (ReqArg lowerRightFunc "LOWERRIGHT") "lower right corner"
  , Option "c" [] (ReqArg cFunc "UPPERLEFT") "constant c"
  , Option "z" [] (ReqArg zFunc "UPPERLEFT") "constant z"
  , Option "r" [] (ReqArg rFunc "UPPERLEFT") "constant r"
  , Option "p" [] (ReqArg pFunc "UPPERLEFT") "constant p"
  ]
  where
    typeFunc t opt =
      opt {
        optFractal = case t of
          "mandelbrot"  -> Mandelbrot
          "julia"       -> Julia
          "burningship" -> BurningShip
          "newton"      -> Newton
      }
    sizeFunc arg opt = opt { optSize = parseSize arg }
    upperLeftFunc arg opt = opt { optUpperLeft = parseComplex arg }
    lowerRightFunc arg opt = opt { optLowerRight = parseComplex arg }
    cFunc arg opt = opt { optC = parseComplex arg }
    zFunc arg opt = opt { optZ = parseComplex arg }
    rFunc arg opt = opt { optR = parseComplex arg }
    pFunc arg opt = opt { optP = parseComplex arg }
    seedFunc arg opt = opt { optSeed = read arg }
    colorFunc color opt =
      opt { 
        optColor = case color of
          "bw"     -> blackOnWhite
          "wb"     -> whiteOnBlack
          "gray"   -> grayScale
          "red"    -> redScale
          "green"  -> greenScale
          "blue"   -> blueScale
          "random" -> randomColors (randomColorsGenerator (optSeed opt))
      }
      
parseSize str =
  case regexMatch of
    Nothing      -> error $ show str ++ " not a valid size (width x height)"
    Just matches -> let vals = map read matches
                    in Dimension (vals !! 0) (vals !! 1)
    where
      regexMatch = matchRegex (mkRegex "^([0-9.]+)x([0-9.]+)$") str  

parseComplex str = 
  case regexMatch of
    Nothing      -> error $ show str ++ " not a valid complex number"
    Just matches -> let vals = map read matches
                    in (vals !! 0) :+ (vals !! 1)
    where
      regexMatch = matchRegex (mkRegex "^([-]?[0-9.]+)[+]([-]?[0-9.]+)i$") str