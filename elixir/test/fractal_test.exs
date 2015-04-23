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
      color:       :blue,
      size:        %Mandelbrot.Size{ width: 30, height: 20 },
      upper_left:  %Complex{ real: -2.0, imag:  1.0 },
      lower_right: %Complex{ real:  1.0, imag: -1.0 }
    }

    ppm = Mandelbrot.Fractal.generate(options)
    # 4 lines of header + 30 columns * 20 rows
    assert 604 = Enum.count(String.split(ppm, "\n"))
  end

  test "fractal_iterate" do
    import Mandelbrot.Fractal, only: [ fractal_iterate: 3 ]

    next = &Mandelbrot.NextFunction.mandelbrot_next/2

    z0 = %Complex{ real: 0.0, imag: 0.0 }
    c  = %Complex{ real: 1.0, imag: 0.0 }
    assert { %Complex{ real: 2.0, imag: 0.0 },   3 } == fractal_iterate(next, z0, c)

    z0 = %Complex{ real: 0.0, imag: 0.0 }
    c  = %Complex{ real: 0.0, imag: 0.0 }
    assert { %Complex{ real: 0.0, imag: 0.0 }, 256 } == fractal_iterate(next, z0, c)
  end

  test "in_or_out" do
    import Mandelbrot.Fractal, only: [ in_or_out: 1 ]

    assert { :inside,  :meh, 256 } == in_or_out({ :meh, 256 })
    assert { :inside,  :meh, 538 } == in_or_out({ :meh, 538 })
    assert { :outside, :meh, 255 } == in_or_out({ :meh, 255 })
    assert { :outside, :meh,  34 } == in_or_out({ :meh,  34 })
  end

end
