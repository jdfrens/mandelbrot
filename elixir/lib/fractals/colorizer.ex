defmodule Fractals.Colorizer do
  alias Fractals.Options

  # TEMPORARY DEFINITION!!!!!
  # So temporary that I didn't even definen specs for it.

  # This undoes what Grid does: turns the complex number back into
  # row and column and then scales them for a purplish picture.
  #
  # No practical use for fractals themselves, but generates a pretty
  # enough picture for now.

  def color_point(%Complex{real: real, imag: imag}, options) do
    %Options{
      size:        size,
      upper_left:  %Complex{real: x0, imag: y0},
      lower_right: %Complex{real: x1, imag: y1}
    } = options
    red  = ungrid(real, size.width,  x0, x1)
    blue = ungrid(imag, size.height, y1, y0)
    PPM.ppm(red, 0, blue)
  end

  def ungrid(value, count, first, last) do
    ij = trunc((value - first) * (count - 1) / (last - first))
    div(rem(ij, 1024), 4)
  end
end
