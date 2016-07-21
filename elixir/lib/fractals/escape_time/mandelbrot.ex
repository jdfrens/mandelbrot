defmodule Fractals.EscapeTime.Mandelbrot do
  import Complex

  use Fractals.EscapeTime

  def iterate(grid_point, _params) do
    Stream.iterate(Complex.zero, &iterator(&1, grid_point))
  end

  def iterator(z, c) do
    z |> square |> add(c)
  end
end
