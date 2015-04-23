defmodule Mandelbrot.Fractal.Test do

  use ExUnit.Case, async: true

  test "generate_header" do
    options = %Mandelbrot.Options{ size: %Mandelbrot.Size{ width: 55, height: 99 } }
    assert ["P3", "55", "99", "255"] == Mandelbrot.Fractal.generate_header(options)
  end

  test "generate" do
    # more of a property test
    import Mandelbrot.Fractal, only: [ generate: 1 ]

    options       = %Mandelbrot.Options{
      fractal:     :mandelbrot,
      size:        %Mandelbrot.Size{ width: 30, height: 20 },
      upper_left:  %Complex{ real: -2.0, imag:  1.0 },
      lower_right: %Complex{ real:  1.0, imag: -1.0 }
    }

    ppm = Mandelbrot.Fractal.generate(options)
    assert 4 + 30 * 20 = Enum.count(String.split(ppm, "\n"))
  end

  test "generate_grid" do
    import Mandelbrot.Fractal, only: [ generate_grid: 1 ]

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

  test "float_sequence" do
    assert [-1.0, 0.0, 1.0] == Mandelbrot.Fractal.float_sequence(3, -1.0, 1.0)
    assert [-2.0, -0.75, 0.5, 1.75, 3.0] == Mandelbrot.Fractal.float_sequence(5, -2.0, 3.0)
    assert [1.0, 0.0, -1.0] == Mandelbrot.Fractal.float_sequence(3, 1.0, -1.0)
  end

  test "fractal_iterate" do
    import Mandelbrot.Fractal, only: [ fractal_iterate: 2 ]

    z0 = %Complex{ real: 0.0, imag: 0.0 }
    c  = %Complex{ real: 1.0, imag: 0.0 }
    assert { %Complex{ real: 2.0, imag: 0.0 },   3 } == fractal_iterate(z0, c)

    z0 = %Complex{ real: 0.0, imag: 0.0 }
    c  = %Complex{ real: 0.0, imag: 0.0 }
    assert { %Complex{ real: 0.0, imag: 0.0 }, 256 } == fractal_iterate(z0, c)
  end

  test "in_or_out" do
    import Mandelbrot.Fractal, only: [ in_or_out: 1 ]

    assert { :inside,  :whatever } == in_or_out({ :whatever, 256 })
    assert { :inside,  :whatever } == in_or_out({ :whatever, 538 })
    assert { :outside, :whatever } == in_or_out({ :whatever, 255 })
    assert { :outside, :whatever } == in_or_out({ :whatever,  34 })
  end

  test "mandelbrot_next" do
    z = %Complex{ real: 3.0, imag: 2.0 }
    c = %Complex{ real: 1.0, imag: 1.0 }
    assert %Complex{ real: 6.0, imag: 13.0 } == Mandelbrot.Fractal.mandelbrot_next(z, c)
  end
end
