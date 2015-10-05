defmodule Fractals.Color.Simple do
  @moduledoc """
  Provides very simple colors like black-on-white, white-on-black,
  and grayscale.
  """

  alias Fractals.Generator
  alias Fractals.Color

  def black_on_white({  :inside, _, _ }), do: PPM.black
  def black_on_white({ :outside, _, _ }), do: PPM.white

  def white_on_black({  :inside, _, _ }), do: PPM.white
  def white_on_black({ :outside, _, _ }), do: PPM.black

  def gray({ :inside, _, _ }), do: PPM.black
  # FIXME: what the hell is this actually computing?
  def gray({ :outside, _, iterations }) do
    factor = :math.sqrt(iterations / Generator.max_iterations)
    intensity = round(Color.max_intensity * factor * factor)
    PPM.ppm(intensity, intensity, intensity)
  end

end
