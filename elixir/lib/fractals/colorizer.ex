defmodule Fractals.Colorizer do
  # NOTE: this implements the very simple black-on-white coloring
  # TODO: write other colorizers and figure out a good naming scheme

  alias Fractals.Options
  alias Fractals.Colorizer.{Random,WarpPov}

  @maximum_iterations 256

  defmacro escaped?(iterations) do
    quote do
      unquote(iterations) >= @maximum_iterations
    end
  end

  def color_point({_, iterations}, %Options{color: color}) do
    case color do
      :black_on_white -> black_on_white(iterations)
      :white_on_black -> white_on_black(iterations)
      :gray           -> gray(iterations)
      :red            -> WarpPov.red(iterations)
      :green          -> WarpPov.green(iterations)
      :blue           -> WarpPov.blue(iterations)
      :random         -> Random.at(Random, iterations)
    end
  end

  def black_on_white(iterations) when escaped?(iterations), do: PPM.black
  def black_on_white(_), do: PPM.white

  def white_on_black(iterations) when escaped?(iterations), do: PPM.white
  def white_on_black(_), do: PPM.black

  def gray(iterations) when escaped?(iterations), do: PPM.black
  def gray(iterations) do
    factor = :math.sqrt(iterations / @maximum_iterations)
    intensity = round(@maximum_iterations * factor * factor)
    PPM.ppm(intensity, intensity, intensity)
  end
end
