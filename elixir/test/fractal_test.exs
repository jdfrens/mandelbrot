defmodule Mandelbrot.Fractal.Test do

  use ExUnit.Case, async: true

  test "generate_header" do
    options = %Mandelbrot.Options{ size: [ width: 55, height: 99 ] }
    assert ["P3", "55", "99", "255"] == Mandelbrot.Fractal.generate_header(options)
  end

  test "generate_image" do
    options       = %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ width: 3, height: 4 },
      upper_left:  %Complex{ real: -1.0, imag:  1.0 },
      lower_right: %Complex{ real:  1.0, imag: -1.0 }
    }
    red_generator = fn (_, _) -> "255 0 0" end
    assert List.duplicate("255 0 0", 12) == Mandelbrot.Fractal.generate_image(options, red_generator)
  end

  test "xs" do
    options = %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ width: 3 },
      upper_left:  %Complex{ real: -1.0 },
      lower_right: %Complex{ real:  1.0 }
    }
    assert [-1.0, 0.0, 1.0] == Mandelbrot.Fractal.xs(options)
  end

  test "ys" do
    options = %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ height: 5 },
      upper_left:  %Complex{ imag:  1.0 },
      lower_right: %Complex{ imag: -1.0 }
    }
    assert [1.0, 0.5, 0.0, -0.5, -1.0] == Mandelbrot.Fractal.ys(options)
  end

  test "float_iterate" do
    assert [-1.0, 0.0, 1.0] == Mandelbrot.Fractal.float_iterate(3, -1.0, 1.0)
    assert [-2.0, -0.75, 0.5, 1.75, 3.0] == Mandelbrot.Fractal.float_iterate(5, -2.0, 3.0)
    assert [1.0, 0.0, -1.0] == Mandelbrot.Fractal.float_iterate(3, 1.0, -1.0)
  end

end
