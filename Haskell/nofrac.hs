{-
  No warranty.  No guarantees of any sort.
  Creative Commons Attribution-Share Alike 3.0 United States License
-}

module Main where

import System.Environment
import System.IO
import System.Console.GetOpt
import Control.Monad
import Control.Parallel.Strategies
import Data.Yaml

import Fractals
import Fractals.Options
import Fractals.Color

main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs

  parsedOptions <- decodeFractalFile inputFile
  case parsedOptions of
    Right options -> do
      let colors = computeFractal options
      output <- openFile outputFile WriteMode
      mapM_ (hPutStrLn output) $ ppmPrefix (size options)
      mapM_ (mapM_ $ hPutStrLn output) $ colors
      hClose output
    Left error -> hPutStrLn stderr $ show error

computeFractal options =
  let
    grid = complex_grid (size options) (upperLeft options) (lowerRight options)
  in
    map2 colorFunction $ map2 plotter grid
    where
      plotter =
        case (fractal options) of
          Mandelbrot  -> mandelbrot
          BurningShip -> burningShip
          Julia       -> julia (c options)
          Newton      -> newton (\z -> z^3 - 1) (\z -> 3.0 * z^2)
      colorFunction = case (color options) of
                           BlackOnWhite -> blackOnWhite
                           WhiteOnBlack -> whiteOnBlack
                           Gray         -> grayScale
                           Red          -> redScale
                           Green        -> greenScale
                           Blue         -> blueScale
                           Random       -> randomColors $ randomColorsGenerator $ seed options

map2 f l = map (parMap rpar f) l
