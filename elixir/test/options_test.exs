defmodule Mandelbrot.Options.Test do

  use ExUnit.Case, async: true

  import Mandelbrot.Options, only: [ parse: 1 ]

  test "parse" do
    json = """
    {
      "fractal":    "Mandelbrot",
      "size":       "720x480",
      "color":      "BlackOnWhite",
      "seed":       12345,
      "upperLeft":  "0.0+55.2i",
      "lowerRight": "92.3+120.3i",
      "c":          "3.14+4.13i",
      "z":          "4.4+1.1i",
      "r":          "9.9+3.3i",
      "p":          "0.3+0.5i"
    }
    """
    assert parse(json) == %Mandelbrot.Options{
                            fractal: :mandelbrot,
                            size:        [ width: 720, height: 480 ],
                            color:       :black_on_white,
                            seed:        12345,
                            upper_left:  %Complex{ real: 0.0, imag: 55.2 },
                            lower_right: %Complex{ real: 92.3, imag: 120.3 },
                            c:           %Complex{ real: 3.14, imag: 4.13 },
                            z:           %Complex{ real: 4.4,  imag: 1.1 },
                            r:           %Complex{ real: 9.9,  imag: 3.3 },
                            p:           %Complex{ real: 0.3,  imag: 0.5 }
                          }
  end

  test "parse with defaults" do
    json = """
    {
      "fractal":    "Julia",
      "upperLeft":  "5.0+6.0i",
      "lowerRight": "6.0+5.0i"
    }
    """
    expected = %Mandelbrot.Options {
      fractal:     :julia,
      size:        [ width: 512, height: 384 ],
      color:       :black_on_white,
      seed:        666,
      upper_left:  %Complex{ real: 5.0, imag: 6.0 },
      lower_right: %Complex{ real: 6.0, imag: 5.0 },
      c:           %Complex{ real: 1.0, imag: 0.0 },
      z:           %Complex{ real: 0.0, imag: 0.0 },
      r:           %Complex{ real: 0.0, imag: 0.0 },
      p:           %Complex{ real: 0.0, imag: 0.0 }
      }
    assert parse(json) == expected
  end

end
