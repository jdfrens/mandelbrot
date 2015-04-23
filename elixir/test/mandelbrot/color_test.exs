defmodule Mandelbrot.Color.Test do

  use ExUnit.Case, async: true

  test "ppm" do
    assert "  0   0 255 " == Mandelbrot.Color.ppm(0, 0, 255)
  end

  test "scaled_blue" do
    import Mandelbrot.Color, only: [ black: 0, scaled_blue: 1, ppm: 3 ]

    assert black() == scaled_blue({:inside, :meh, :meh})

    # FIXME: what about 0 iterations?
    assert ppm(  0,   0,   0) == scaled_blue({ :outside, :meh,   1 })
    assert ppm(  0,   0,   2) == scaled_blue({ :outside, :meh,   2 })

    assert ppm(  0,   0, 249) == scaled_blue({ :outside, :meh, 126 })
    assert ppm(  0,   0, 251) == scaled_blue({ :outside, :meh, 127 })
    assert ppm(  0,   0, 255) == scaled_blue({ :outside, :meh, 128 })
    assert ppm(  2,   2, 255) == scaled_blue({ :outside, :meh, 129 })

    assert ppm(253, 253, 255) == scaled_blue({ :outside, :meh, 255 })
    assert ppm(255, 255, 255) == scaled_blue({ :outside, :meh, 256 })
  end
end
