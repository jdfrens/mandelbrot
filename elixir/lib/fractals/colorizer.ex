defmodule Fractals.Colorizer do
  # NOTE: this implements the very simple black-on-white coloring
  # TODO: write other colorizers and figure out a good naming scheme

  alias Fractals.Options

  @maximum_iterations 256

  defmacro escaped?(iterations) do
    quote do
      unquote(iterations) >= @maximum_iterations
    end
  end

  def color_point({_, iterations}, %Options{color: :black_on_white}) when escaped?(iterations) do
    PPM.black
  end
  def color_point(_, %Options{color: :black_on_white}), do: PPM.white

  def color_point({_, iterations}, %Options{color: :white_on_black}) when escaped?(iterations) do
    PPM.white
  end
  def color_point(_, %Options{color: :white_on_black}), do: PPM.black

  def color_point({_, iterations}, %Options{color: :gray}) when escaped?(iterations) do
    PPM.black
  end
  def color_point({_, iterations}, %Options{color: :gray}) do
    factor = :math.sqrt(iterations / @maximum_iterations)
    intensity = round(@maximum_iterations * factor * factor)
    PPM.ppm(intensity, intensity, intensity)
  end
end
