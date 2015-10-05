defmodule Fractals.Color.WarpPov do
  @moduledoc """
  Provides color functions for each of the primary colors (red, green,
  and blue) which uses the brighted hue at the edge of a fractal, quickly
  tapering off to black as you get farther away.

  It's a cool effect, but not the most colorful.

  Taken from http://warp.povusers.org/Mandelbrot/
  """

  import PPM, only: [ ppm: 3 ]
  import Fractals.Color, only: [ max_intensity: 0 ]
  import Fractals.Generator, only: [ max_iterations: 0 ]

  def red(pixel) do
    permute_red(hues(pixel))
  end

  def green(pixel) do
    permute_green(hues(pixel))
  end

  def blue(pixel) do
    permute_blue(hues(pixel))
  end

  def permute_red({ primary, secondary }) do
    ppm(primary, secondary, secondary)
  end
  def permute_green({ primary, secondary }) do
    ppm(secondary, primary, secondary)
  end
  def permute_blue({ primary, secondary }) do
    ppm(secondary, secondary, primary)
  end

  def hues({ :inside,  _, _ }), do: { 0 , 0 }
  def hues({ :outside, _, iterations }) do
    half_iterations = max_iterations/2-1
    if iterations <= half_iterations do
      { scale(max(1, iterations)), 0 }
    else
      { max_intensity, scale(iterations - half_iterations) }
    end
  end

  def scale(i) do
    round(2.0 * (i - 1) / max_iterations * max_intensity)
  end

end
