defmodule Fractals.Colorizer.WarpPov do
  @moduledoc """
  Provides color functions for each of the primary colors (red, green,
  and blue) which uses the brighted hue at the edge of a fractal, quickly
  tapering off to black as you get farther away.

  It's a cool effect, but not the most colorful.

  Taken from http://warp.povusers.org/Mandelbrot/
  """

  alias Fractals.Params

  import Fractals.EscapeTime.Helpers

  @spec red(non_neg_integer, Params.t()) :: String.t()
  def red(iterations, params) do
    permute_red(intensities(iterations, params))
  end

  @spec green(non_neg_integer, Params.t()) :: String.t()
  def green(iterations, params) do
    permute_green(intensities(iterations, params))
  end

  @spec blue(non_neg_integer, Params.t()) :: String.t()
  def blue(iterations, params) do
    permute_blue(intensities(iterations, params))
  end

  @spec permute_red({non_neg_integer, non_neg_integer}) :: String.t()
  def permute_red({primary, secondary}) do
    PPM.ppm(primary, secondary, secondary)
  end

  @spec permute_green({non_neg_integer, non_neg_integer}) :: String.t()
  def permute_green({primary, secondary}) do
    PPM.ppm(secondary, primary, secondary)
  end

  @spec permute_blue({non_neg_integer, non_neg_integer}) :: String.t()
  def permute_blue({primary, secondary}) do
    PPM.ppm(secondary, secondary, primary)
  end

  @spec intensities(non_neg_integer, Params.t()) :: {non_neg_integer, non_neg_integer}
  def intensities(iterations, %Params{max_iterations: max_iterations})
      when inside?(iterations, max_iterations),
      do: {0, 0}

  def intensities(iterations, params) do
    half_iterations = params.max_iterations / 2 - 1

    if iterations <= half_iterations do
      {scale(max(1, iterations), params), 0}
    else
      {params.max_intensity, scale(iterations - half_iterations, params)}
    end
  end

  @spec scale(float, Params) :: non_neg_integer
  def scale(i, params) do
    round(2.0 * (i - 1) / params.max_iterations * params.max_intensity)
  end
end
