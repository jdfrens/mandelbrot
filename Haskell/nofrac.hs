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

import Options
import PPM
import Fractals

main :: IO ()
main = do
  [inputFile, outputFile] <- getArgs

  yamlOptions <- join $ decodeFile inputFile
  optionsMapping  <- fromMapping yamlOptions
  let opts = parseYamlOptions optionsMapping

  let colors = computeFractal opts

  output <- openFile outputFile WriteMode
  mapM_ (hPutStrLn output) $ ppmPrefix (optSize opts)
  mapM_ (mapM_ $ hPutStrLn output) $ colors
  hClose output

computeFractal opts =
  let
    grid = complex_grid (optSize opts) (optUpperLeft opts) (optLowerRight opts)
  in
    map2 (optColor opts) $ map2 (plotter opts) grid
    where
      plotter opts =
        case (optFractal opts) of
          Mandelbrot  -> mandelbrot
          BurningShip -> burningShip
          Julia       -> julia (optC opts)
          Newton      -> newton (\z -> z^3 - 1) (\z -> 3.0 * z^2)

map2 f l = parMap rwhnf (map f) l
