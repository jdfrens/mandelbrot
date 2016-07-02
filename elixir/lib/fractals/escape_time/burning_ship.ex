defmodule Fractals.EscapeTime.BurningShip do
  @moduledoc """
  Kind of an awesome fractal.  It looks a bit like a burning ship.
  """

  import Complex

  @magnitude_cutoff         2.0
  @magnitude_cutoff_squared 4.0
  @max_iterations           256

  def max_iterations, do: @max_iterations

  def pixels(grid_points) do
    Enum.map(grid_points, &escape_time(Complex.zero, &1))
  end

  def escaped?(z) do
    Complex.magnitude_squared(z) >= @magnitude_cutoff_squared
  end

  def escape_time(grid_point, c) do
    grid_point
    |> Stream.iterate(&iterator(&1,c))
    |> Stream.with_index
    |> Stream.drop_while(fn {z, i} -> !escaped?(z) && i < @max_iterations end)
    |> Stream.take(1)
    |> Enum.to_list
    |> List.first
  end

  def iterator(z, c) do
    z |> burn |> square |> add(c)
  end

  def burn(%Complex{real: real, imag: imag}) do
    # FIXME: not sure why I need to negate imag since Wikipedia doesn't
    %Complex{real: abs(real), imag: -1 * abs(imag)}
  end
end
