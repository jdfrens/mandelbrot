defmodule Mandelbrot.Color do

  alias Mandelbrot.Color.Random
  alias Mandelbrot.Color.Simple
  alias Mandelbrot.Color.WarpPov

  # FIXME: or is this better in PPM?
  @max_intensity 255
  def max_intensity, do: @max_intensity

  def color_function(options) do
    case options.color do
      :black_on_white -> &Simple.black_on_white/1
      :white_on_black -> &Simple.white_on_black/1
      :gray           -> &Simple.gray/1
      :blue           -> &WarpPov.blue/1
      :green          -> &WarpPov.green/1
      :red            -> &WarpPov.red/1
      :random         -> Random.build_random
    end
  end

end
