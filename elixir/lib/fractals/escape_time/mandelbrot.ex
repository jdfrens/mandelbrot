defmodule Fractals.EscapeTime.Mandelbrot do
  import Complex

  @magnitude_cutoff         2.0
  @magnitude_cutoff_squared 4.0
  @max_iterations           256

  def max_iterations, do: @max_iterations

  def pixels(grid_points) do
    Enum.map(grid_points, &escape_time(&1))
  end

  def escaped?(z) do
    Complex.magnitude_squared(z) >= @magnitude_cutoff_squared
  end

  def escape_time(grid_point) do
    Complex.zero
    |> Stream.iterate(&iterator(&1,grid_point))
    |> Stream.with_index
    |> Stream.drop_while(fn {z, i} -> !escaped?(z) && i < @max_iterations end)
    |> Stream.take(1)
    |> Enum.to_list
    |> List.first
  end

  def iterator(z, c) do
    z |> square |> add(c)
  end
end
