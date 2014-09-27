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
import qualified Data.HashMap.Lazy as Map
import System.IO.Unsafe

import Debug.Trace

import Fractals (FractalType(..), Dimension(..))
import Fractals.Color (ColorMap(..))

decodemf :: (FromJSON a) => String -> Either String a
decodemf = Y.decodeEither . C.pack

encodemf :: (ToJSON a) => a -> String
encodemf = C.unpack . Y.encode

decodeFractalFile :: FilePath -> IO (Either ParseException Options)
decodeFractalFile = Y.decodeFileEither

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

instance ToJSON Options where
  toJSON = Object . optionsAsHash
    where optionsAsHash = Map.fromList . optionsAsList
          optionsAsList opts = [
            convert ("fractal",    fractal opts),
            convert ("size",       size opts),
            convert ("color",      color opts),
            convert ("seed",       seed opts),
            convert ("upperLeft",  upperLeft opts),
            convert ("lowerRight", lowerRight opts),
            convert ("c",          c opts),
            convert ("z",          z opts),
            convert ("r",          r opts),
            convert ("p",          p opts)
            ]
          convert (k, v) = (T.pack k, toJSON v)

instance FromJSON Dimension where
  parseJSON (String s) = return $ parseSize $ T.unpack s
    where parseSize str =
            case regexMatch str of
              Nothing      -> error $ show str ++ " not a valid size (width x height)"
              Just matches -> let vals = map read matches
                              in Dimension (vals !! 0) (vals !! 1)
          regexMatch = matchRegex (mkRegex "^([0-9.]+)x([0-9.]+)$")

instance ToJSON Dimension where
  toJSON = String . T.pack . show

instance FromJSON FractalType where
  parseJSON (String s) = return $ parseFractalType $ T.unpack s
  parseJSON _          = mzero

instance ToJSON FractalType where
  toJSON = String . T.pack . show

parseFractalType :: String -> FractalType
parseFractalType word =
  case word of
    "Mandelbrot"  -> Mandelbrot
    "Julia"       -> Julia
    "BurningShip" -> BurningShip
    "Newton"      -> Newton
    "Nova"        -> Nova
    _             -> throw $ AesonException $ word ++ " is not a valid fractal type"

instance FromJSON ColorMap where
  parseJSON (String s) = return $ parseColorMap $ T.unpack s
  parseJSON _          = mzero

instance ToJSON ColorMap where
  toJSON = String . T.pack . show

parseColorMap word =
  case word of
    "BlackOnWhite" -> BlackOnWhite
    "WhiteOnBlack" -> WhiteOnBlack
    "Gray"         -> Gray
    "Red"          -> Red
    "Blue"         -> Blue
    "Green"        -> Green
    "Random"       -> Random
    _             -> throw $ AesonException $ word ++ " is not a valid color map"

instance (Read a) => FromJSON (Complex a) where
  parseJSON (String s) = return $ parseComplex $ T.unpack s
  parseJSON _          = mzero

instance (Show a, RealFloat a) => ToJSON (Complex a) where
  toJSON z = String $ T.pack $ a ++ "+" ++ b ++ "i"
            where a = show (realPart z)
                  b = show (imagPart z)

-- TODO: rewrite with attoparsec?
parseComplex str =
  case regexMatch of
    Nothing      -> error $ show str ++ " not a valid complex number"
    Just matches -> let vals = map read matches
                    in (vals !! 0) :+ (vals !! 2)
    where
      pattern = "^" ++
                "([-]?[0-9.]+(e[-]?[0-9]+)?)" ++
                "[+]" ++
                "([-]?[0-9.]+(e[-]?[0-9]+)?)i" ++
                "$"
      regexMatch = matchRegex (mkRegex pattern) str
