defmodule Fractals.Colorizer.BlackAndWhiteAndGray do
  @moduledoc """
  Colorizers for black-on-white, white-on-black, and grayscale.
  """

  import Fractals.EscapeTime.Helpers

  alias Fractals.Params

  @spec black_on_white(integer, Params.t()) :: PPM.color()
  def black_on_white(iterations, %Params{max_iterations: max_iterations})
      when inside?(iterations, max_iterations),
      do: PPM.black()

  def black_on_white(_, _), do: PPM.white()

  @spec white_on_black(integer, Params.t()) :: PPM.color()
  def white_on_black(iterations, %Params{max_iterations: max_iterations})
      when inside?(iterations, max_iterations),
      do: PPM.white()

  def white_on_black(_, _), do: PPM.black()

  @spec gray(integer, Params.t()) :: PPM.color()
  def gray(iterations, %Params{max_iterations: max_iterations})
      when inside?(iterations, max_iterations),
      do: PPM.black()

  def gray(iterations, params) do
    factor = :math.sqrt(iterations / params.max_iterations)
    intensity = round(params.max_intensity * factor)
    PPM.ppm(intensity, intensity, intensity)
  end
end
