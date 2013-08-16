{-# LANGUAGE OverloadedStrings #-}

{-
  No warranty.  No guarantees of any sort.
  Creative Commons Attribution-Share Alike 3.0 United States License
-}

module Fractals.Options
where

import Control.Applicative
import Control.Monad
import Control.Exception.Base
import System.Console.GetOpt
import Data.Complex
import Data.Yaml as Y
import Text.Regex
import qualified Data.Text as T
import qualified Data.ByteString.Char8 as C
import System.IO.Unsafe

import Debug.Trace

import Fractals
import Fractals.Color

data FractalType = Mandelbrot | Julia | BurningShip | Newton
  deriving (Eq, Show, Read)

instance FromJSON Dimension where
  parseJSON (String s) = return $ parseSize $ T.unpack s
    where parseSize str =
            case regexMatch str of
              Nothing      -> error $ show str ++ " not a valid size (width x height)"
              Just matches -> let vals = map read matches
                              in Dimension (vals !! 0) (vals !! 1)
          regexMatch = matchRegex (mkRegex "^([0-9.]+)x([0-9.]+)$")

instance FromJSON FractalType where
  parseJSON (String s) = return $ parseFractalType $ T.unpack s
  parseJSON _          = mzero

instance FromJSON ColorMap where
  -- FIXME: write a real definition
  parseJSON _ = return BlackOnWhite

parseFractalType :: String -> FractalType
parseFractalType word =
  case word of
    "Mandelbrot"  -> Mandelbrot
    "Julia"       -> Julia
    "BurningShip" -> BurningShip
    "Newton"      -> Newton
    _             -> throw $ AesonException $ word ++ " is not a valid fractal type"

instance (Read a) => FromJSON (Complex a) where
  parseJSON (String s) = return $ parseComplex $ T.unpack s
  parseJSON _          = mzero

data Options = Options {
  fractal    :: FractalType,
  size       :: Dimension,
  color      :: ColorMap,
  seed       :: Int,
  upperLeft  :: Complex Double,
  lowerRight :: Complex Double,
  c          :: Complex Double,
  z          :: Complex Double,
  r          :: Complex Double,
  p          :: Complex Double
  }
  deriving (Eq, Show)

instance FromJSON Options where
    parseJSON (Object v) =
        Options <$> v .:  "fractal"
                <*> v .:? "size"       .!= Dimension 512 384
                <*> v .:? "color"      .!= BlackOnWhite
                <*> v .:? "seed"       .!= 666
                <*> v .:  "upperLeft"
                <*> v .:  "lowerRight"
                <*> v .:? "c"          .!= (1.0 :+ 0.0)
                <*> v .:? "z"          .!= (0.0 :+ 0.0)
                <*> v .:? "r"          .!= (0.0 :+ 0.0)
                <*> v .:? "p"          .!= (0.0 :+ 0.0)
    parseJSON _ = mzero

decodemf :: (FromJSON a) => String -> Either String a
decodemf = Y.decodeEither . C.pack

decodeFractalFile :: FilePath -> IO (Either ParseException Options)
decodeFractalFile = Y.decodeFileEither

-- parseColor color seed =
--     case color of
--           "bw"     -> blackOnWhite
--           "wb"     -> whiteOnBlack
--           "gray"   -> grayScale
--           "red"    -> redScale
--           "green"  -> greenScale
--           "blue"   -> blueScale
--           "random" -> randomColors (randomColorsGenerator seed)

parseComplex str =
  case regexMatch of
    Nothing      -> error $ show str ++ " not a valid complex number"
    Just matches -> let vals = map read matches
                    in (vals !! 0) :+ (vals !! 1)
    where
      regexMatch = matchRegex (mkRegex "^([-]?[0-9.]+)[+]([-]?[0-9.]+)i$") str
