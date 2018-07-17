defmodule Fractals.EscapeTimeTest do
  use ExUnit.Case, async: true

  import Complex

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
    grid_point = cmplx(1.0)
    params = %{Params.default() | c: cmplx(1.0)}
    assert EscapingIteration.pixels([grid_point], params) == [{cmplx(2.0), 1}]
  end

  test "uses grid point and params" do
    grid_point = cmplx(0.0)
    params = %{Params.default() | c: cmplx(0.5)}
    assert EscapingIteration.pixels([grid_point], params) == [{cmplx(2.0), 4}]
  end

  test "stops when max iterations reached" do
    grid_point = cmplx(1.0)
    params = Params.default()
    assert InsideIteration.pixels([grid_point], params) == [{cmplx(1.0), 256}]
  end

  test "iterates over all grid points" do
    grid_points = [cmplx(0.5), cmplx(-1.0)]
    params = %{Params.default() | c: cmplx(1.0)}
    assert EscapingIteration.pixels(grid_points, params) == [{cmplx(2.5), 2}, {cmplx(2.0), 3}]
  end
end
