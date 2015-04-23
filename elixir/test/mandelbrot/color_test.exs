defmodule Mandelbrot.Color.Test do

  use ExUnit.Case, async: true

  test "ppm" do
    assert "  0   0 255 " == Mandelbrot.Color.ppm(0, 0, 255)
  end

  test "pov_scale" do
    import Mandelbrot.Color, only: [ black: 0, pov_scale: 3 ]

    assert black() == pov_scale({:inside, :meh, :meh}, :meh, :meh)

    plateau = &("plateau #{&1}")
    border = &("border #{&1}")

    # FIXME: what about 0 iterations?
    assert "plateau 0" == pov_scale({ :outside, :meh,   1 }, plateau, border)
    assert "plateau 2" == pov_scale({ :outside, :meh,   2 }, plateau, border)

    assert "plateau 249" == pov_scale({ :outside, :meh, 126 }, plateau, border)
    assert "plateau 251" == pov_scale({ :outside, :meh, 127 }, plateau, border)
    assert "border 0" == pov_scale({ :outside, :meh, 128 }, plateau, border)
    assert "border 2" == pov_scale({ :outside, :meh, 129 }, plateau, border)

    assert "border 253" == pov_scale({ :outside, :meh, 255 }, plateau, border)
    assert "border 255" == pov_scale({ :outside, :meh, 256 }, plateau, border)
  end

  test "scaled_blue" do
    import Mandelbrot.Color, only: [ scaled_blue: 1, ppm: 3 ]

    assert ppm(  0,   0,   0) == scaled_blue({ :outside, :meh,   1 })
    assert ppm(  0,   0, 251) == scaled_blue({ :outside, :meh, 127 })
    assert ppm(  0,   0, 255) == scaled_blue({ :outside, :meh, 128 })
    assert ppm(255, 255, 255) == scaled_blue({ :outside, :meh, 256 })
  end

  test "scaled_green" do
    import Mandelbrot.Color, only: [ scaled_green: 1, ppm: 3 ]

    assert ppm(  0,   0,   0) == scaled_green({ :outside, :meh,   1 })
    assert ppm(  0, 251,   0) == scaled_green({ :outside, :meh, 127 })
    assert ppm(  0, 255,   0) == scaled_green({ :outside, :meh, 128 })
    assert ppm(255, 255, 255) == scaled_green({ :outside, :meh, 256 })
  end

  test "scaled_red" do
    import Mandelbrot.Color, only: [ scaled_red: 1, ppm: 3 ]

    assert ppm(  0,   0,   0) == scaled_red({ :outside, :meh,   1 })
    assert ppm(251,   0,   0) == scaled_red({ :outside, :meh, 127 })
    assert ppm(255,   0,   0) == scaled_red({ :outside, :meh, 128 })
    assert ppm(255, 255, 255) == scaled_red({ :outside, :meh, 256 })
  end

  test "random" do
    import Mandelbrot.Color, only: [ white: 0, random_color: 1, ppm: 3 ]

    assert white == random_color({ :insider, :meh, :meh })
  end
end
