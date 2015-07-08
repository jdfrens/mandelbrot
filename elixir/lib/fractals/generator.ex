defmodule Fractals.Generator do

  import Stream, only: [ concat: 2, iterate: 2, map: 2, take: 2, drop_while: 2, with_index: 1 ]
  import Complex, only: :macros

  alias Fractals.Iterators
  alias Fractals.Color
  alias Fractals.Grid


  @magnitude_cutoff         2.0
  @magnitude_cutoff_squared 4.0
  @max_iterations           256

  def max_iterations, do: @max_iterations

  def generate(options) do
    concat(header(options), image(options))
  end

  def header(options) do
    %Fractals.Size{ width: width, height: height } = options.size
    PPM.p3_header(width, height)
  end

  def image(options) do
    color_func = Color.color_function(options)
    options
    |> Grid.generate_grid
    |> Enum.map(&build_complex/1)
    |> Enum.map(fn grid_point -> pixel(grid_point, Iterators.build(grid_point, options), color_func) end)
    |> Enum.map(&Task.await/1)
  end
  end

  def build_complex({ r, i }), do: cmplx(r, i)

  def pixel(grid_point, next_func, color_func) do
    iterate(next_func, &cutoff/1, grid_point)
    |> in_or_out
    |> color(color_func)
  end

  def cutoff(z) do
    Complex.magnitude_squared(z) < @magnitude_cutoff_squared
  end

  def color(in_out, color_func) do
    color_func.(in_out)
  end

  def in_or_out({ z, iterations }) do
    status = if iterations >= 255, do: :inside, else: :outside
    { status, z, iterations }
  end

  def iterate(iterator, cutoff, grid_point) do
    grid_point
    |> iterate(iterator)
    |> with_index
    |> drop_while(fn { z, i } -> cutoff.(z) && i < 255 end)
    |> take(1)
    |> Enum.to_list
    |> List.first
  end

end
