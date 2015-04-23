defmodule Mandelbrot.Color do

  # FIXME: DRY it up!
  @max_intensity 255
  @max_iterations  256
  @half_iterations 127

  @black :io_lib.format("~3B ~3B ~3B ", [  0,   0,   0])
  @white :io_lib.format("~3B ~3B ~3B ", [255, 255, 255])
  def black, do: @black
  def white, do: @white

  # TODO: blog about this
  def ppm(red, green, blue) do
    :erlang.iolist_to_binary(:io_lib.format("~3B ~3B ~3B ", [red, green, blue]))
  end

  def color_function(options) do
    case options.color do
      :blue   -> &scaled_blue/1
      :green  -> &scaled_green/1
      :red    -> &scaled_red/1
      :random ->
        # FIXME: :random is wrong
        # -- need to figure out how to generate random numbers well in Elixir
        # -- need to memoize the random colors
        &scaled_blue/1
    end
  end

  def scaled_blue(pixel) do
    pov_scale(pixel,
      &(ppm( 0,  0,             &1)),
      &(ppm(&1, &1, @max_intensity)))
  end

  def scaled_green(pixel) do
    pov_scale(pixel,
      &(ppm( 0,             &1,  0)),
      &(ppm(&1, @max_intensity, &1)))
  end

  def scaled_red(pixel) do
    pov_scale(pixel,
      &(ppm(            &1,  0,  0)),
      &(ppm(@max_intensity, &1, &1)))
  end

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
