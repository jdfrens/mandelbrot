{-
  No warranty.  No guarantees of any sort.
  Creative Commons Attribution-Share Alike 3.0 United States License
-}

module Main where

import System
import System.Console.GetOpt
import Control.Monad
import Control.Parallel.Strategies

import Options
import PPM
import Fractals

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  let opts = foldl (flip ($)) defaultOptions actions
  mapM_ putStrLn $ ppmPrefix (optSize opts)
  let grid = complex_grid (optSize opts) (optUpperLeft opts) (optLowerRight opts)
  mapM_ (mapM_ putStrLn) $ map2 (optColor opts) $ map2 (plotter opts) grid
    where
      plotter opts =
        case (optFractal opts) of
          Mandelbrot  -> mandelbrot
          BurningShip -> burningShip
          Julia       -> julia (optC opts)
          Newton      -> newton (\z -> z^3 - 1) (\z -> 3.0 * z^2)

map2 f l = parMap rwhnf (map f) l