defmodule Fractals.Colorizer.WarpPov do
  @moduledoc """
  Provides color functions for each of the primary colors (red, green,
  and blue) which uses the brighted hue at the edge of a fractal, quickly
  tapering off to black as you get farther away.

  It's a cool effect, but not the most colorful.

  Taken from http://warp.povusers.org/Mandelbrot/
  """

  import Fractals.Colorizer, only: :macros

  @maximum_intensity 255
  @maximum_iterations 256

  @spec red(non_neg_integer) :: String.t
  def red(iterations) do
    permute_red(hues(iterations))
  end

  @spec green(non_neg_integer) :: String.t
  def green(iterations) do
    permute_green(hues(iterations))
  end

  @spec blue(non_neg_integer) :: String.t
  def blue(iterations) do
    permute_blue(hues(iterations))
  end

  @spec permute_red({non_neg_integer, non_neg_integer}) :: String.t
  def permute_red({primary, secondary}) do
    PPM.ppm(primary, secondary, secondary)
  end
  @spec permute_green({non_neg_integer, non_neg_integer}) :: String.t
  def permute_green({primary, secondary}) do
    PPM.ppm(secondary, primary, secondary)
  end
  @spec permute_blue({non_neg_integer, non_neg_integer}) :: String.t
  def permute_blue({primary, secondary}) do
    PPM.ppm(secondary, secondary, primary)
  end

  @spec hues(non_neg_integer) :: {non_neg_integer, non_neg_integer}
  def hues(iterations) when escaped?(iterations), do: {0 , 0}
  def hues(iterations) do
    half_iterations = @maximum_iterations/2-1
    if iterations <= half_iterations do
      {scale(max(1, iterations)), 0}
    else
      {@maximum_intensity, scale(iterations - half_iterations)}
    end
  end

  @spec scale(float) :: non_neg_integer
  def scale(i) do
    round(2.0 * (i - 1) / @maximum_iterations * @maximum_intensity)
  end
end
