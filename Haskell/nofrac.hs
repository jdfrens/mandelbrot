{- 
  For GRHUG
  Version: July 2009
  Creative Commons Attribution-Share Alike 3.0 United States License
-}

module Main where

import System
import System.Console.GetOpt
import Control.Monad

import Options
import PPM
import Fractals

main :: IO ()
main = do
  args <- getArgs
  let (actions, nonOptions, errors) = getOpt RequireOrder options args
  let opts = foldl (flip ($)) defaultOptions actions
  mapM_ putStrLn $ ppmPrefix (optSize opts)
  mapM_ (mapM_ putStrLn) $ color opts $ generate opts

color opts = map (map (optColor opts))

generate opts = 
  plot (plotter opts) (optSize opts) (optUpperLeft opts) (optLowerRight opts)
    where
      plotter opts =
        case (optFractal opts) of
          Mandelbrot  -> mandelbrot
          BurningShip -> burningShip
          Julia       -> julia (optC opts)
          Newton      -> newton (\z -> z^3 - 1) (\z -> 3.0 * z^2)
