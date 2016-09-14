defmodule PPM do
  @moduledoc """
  Helper functions for generating a PPM file---ASCII text to describe
  an image file.  Very verbose, of course, but easy to write and read.

  cf. https://en.wikipedia.org/wiki/Netpbm_format
  """

  @format "~3B ~3B ~3B "

  def black, do: ppm(  0,   0,   0)
  def white, do: ppm(255, 255, 255)

  def p3_header(width, height) do
    [
      "P3",
      Integer.to_string(width),
      Integer.to_string(height),
      "255"
    ]
  end

  def ppm(red, green, blue), do: ppm([red, green, blue])
  def ppm(rgb) do
    :erlang.iolist_to_binary(:io_lib.format(@format, rgb))
  end
end
