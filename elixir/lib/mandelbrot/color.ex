defmodule Mandelbrot.Color do

  @max_intensity 255

  @black :io_lib.format("~3B ~3B ~3B ", [0, 0, 0])
  def black, do: @black

  # TODO: blog about this
  def ppm(red, green, blue) do
    :erlang.iolist_to_binary(:io_lib.format("~3B ~3B ~3B ", [red, green, blue]))
  end

  def color_function(options) do
    case options.color do
      :blue -> &Mandelbrot.Color.scaled_blue/1
    end
  end

  def scaled_blue(pixel) do
    pov_scale(pixel,
      &(ppm(0, 0, &1)),
      &(ppm(&1, &1, @max_intensity)))
  end

  # FIXME: DRY it up!
  @max_iterations  256
  @half_iterations 127

  # FIXME: pick a better name
  def pov_scale({ :inside,  _, _ }, _, _), do: @black
  def pov_scale({ :outside, _, iterations }, plateau, border) do
    if iterations <= @half_iterations do
      plateau.(actual_pov_scale(iterations))
    else
      border.(actual_pov_scale(iterations - @half_iterations))
    end
  end

  # FIXME: pick a better name
  def actual_pov_scale(i) do
    round(2.0 * (i - 1) / @max_iterations * @max_intensity)
  end

end
