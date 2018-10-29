defmodule Fractals.EscapeTimeTest do
  use ExUnit.Case, async: true

  alias Fractals.Params

  defmodule EscapingIteration do
    use Fractals.EscapeTime

    def iterate(grid_point, params) do
      Stream.iterate(grid_point, &Complex.add(&1, params.c))
    end
  end

  defmodule InsideIteration do
    use Fractals.EscapeTime

    def iterate(grid_point, _params) do
      Stream.iterate(grid_point, fn z -> z end)
    end
  end

  test "stops when iteration gets too large" do
    grid_point = Complex.new(1.0)
    params = %{Params.default() | c: Complex.new(1.0)}
    assert EscapingIteration.pixels([grid_point], params) == [{Complex.new(2.0), 1}]
  end

  test "uses grid point and params" do
    grid_point = Complex.new(0.0)
    params = %{Params.default() | c: Complex.new(0.5)}
    assert EscapingIteration.pixels([grid_point], params) == [{Complex.new(2.0), 4}]
  end

  test "stops when max iterations reached" do
    grid_point = Complex.new(1.0)
    params = Params.default()
    assert InsideIteration.pixels([grid_point], params) == [{Complex.new(1.0), 256}]
  end

  test "iterates over all grid points" do
    grid_points = [Complex.new(0.5), Complex.new(-1.0)]
    params = %{Params.default() | c: Complex.new(1.0)}

    assert EscapingIteration.pixels(grid_points, params) == [
             {Complex.new(2.5), 2},
             {Complex.new(2.0), 3}
           ]
  end
end
