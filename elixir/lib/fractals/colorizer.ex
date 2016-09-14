defmodule Fractals.Colorizer do
  # NOTE: this implements the very simple black-on-white coloring
  # TODO: write other colorizers and figure out a good naming scheme

  alias Fractals.Params
  alias Fractals.Colorizer.{Random,WarpPov}

  import Fractals.EscapeTime.Helpers

  @spec color_point({Complex.t, non_neg_integer}, Params) :: String.t
  def color_point({_, iterations}, params) do
    case params.color do
      :black_on_white -> black_on_white(iterations, params.max_iterations)
      :white_on_black -> white_on_black(iterations, params.max_iterations)
      :gray           -> gray(iterations, params)
      :red            -> WarpPov.red(iterations, params)
      :green          -> WarpPov.green(iterations, params)
      :blue           -> WarpPov.blue(iterations, params)
      :random         -> Random.at(Random, iterations, params)
    end
  end

  def black_on_white(iterations, max_iterations) when inside?(iterations, max_iterations), do: PPM.black
  def black_on_white(_,_), do: PPM.white

  def white_on_black(iterations, max_iterations) when inside?(iterations, max_iterations), do: PPM.white
  def white_on_black(_,_), do: PPM.black

  def gray(iterations, %Params{max_iterations: max_iterations}) when inside?(iterations, max_iterations), do: PPM.black
  def gray(iterations, params) do
    factor = :math.sqrt(iterations / params.max_iterations)
    intensity = round(params.max_intensity * factor)
    PPM.ppm(intensity, intensity, intensity)
  end
end
