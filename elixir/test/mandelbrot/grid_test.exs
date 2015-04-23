defmodule Mandelbrot.Grid.Test do

  use ExUnit.Case, async: true

  test "generate_grid" do
    import Mandelbrot.Grid, only: [ generate_grid: 1 ]

    options       = %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ width: 2, height: 3 },
      upper_left:  %Complex{ real: -1.0, imag:  1.0 },
      lower_right: %Complex{ real:  1.0, imag: -1.0 }
    }
    expected = [
      { -1.0,  1.0 }, { 1.0,  1.0 },
      { -1.0,  0.0 }, { 1.0,  0.0 },
      { -1.0, -1.0 }, { 1.0, -1.0 },
    ]

    assert expected == generate_grid(options)
  end

  test "xs" do
    options = %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ width: 3 },
      upper_left:  %Complex{ real: -1.0 },
      lower_right: %Complex{ real:  1.0 }
    }
    assert [-1.0, 0.0, 1.0] == Mandelbrot.Grid.xs(options)
  end

  test "ys" do
    options = %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ height: 5 },
      upper_left:  %Complex{ imag:  1.0 },
      lower_right: %Complex{ imag: -1.0 }
    }
    assert [1.0, 0.5, 0.0, -0.5, -1.0] == Mandelbrot.Grid.ys(options)
  end

  test "float_sequence" do
    import Mandelbrot.Grid, only: [ float_sequence: 3 ]

    assert [-1.0, 0.0, 1.0]              == float_sequence(3, -1.0, 1.0)
    assert [-2.0, -0.75, 0.5, 1.75, 3.0] == float_sequence(5, -2.0, 3.0)
    assert [1.0, 0.0, -1.0]              == float_sequence(3, 1.0, -1.0)
  end

end
