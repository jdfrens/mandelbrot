defmodule PPM do

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

  # TODO: blog about this
  def ppm(red, green, blue) do
    :erlang.iolist_to_binary(:io_lib.format(@format, [red, green, blue]))
  end

end
