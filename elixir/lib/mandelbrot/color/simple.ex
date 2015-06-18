defmodule Mandelbrot.Color.Simple do

  alias Mandelbrot.Fractal
  alias Mandelbrot.Color

  def black_on_white({  :inside, _, _ }), do: PPM.black
  def black_on_white({ :outside, _, _ }), do: PPM.white

  def white_on_black({  :inside, _, _ }), do: PPM.white
  def white_on_black({ :outside, _, _ }), do: PPM.black

  def gray({ :inside, _, _ }), do: PPM.black
  # FIXME: what the hell is this actually computing?
  def gray({ :outside, _, iterations }) do
    factor = :math.sqrt(iterations / Fractal.max_iterations)
    intensity = round(Color.max_intensity * factor * factor)
    PPM.ppm(intensity, intensity, intensity)
  end

end
