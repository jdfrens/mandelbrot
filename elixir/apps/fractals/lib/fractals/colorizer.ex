defmodule Fractals.Colorizer do
  @moduledoc """
  This finds the right colorizer as specified in the params.
  """

  alias Fractals.Colorizer.{BlackAndWhiteAndGray, Random, WarpPov}

  @spec color_point({Complex.complex(), non_neg_integer}, Fractals.Params.t()) :: PPM.color()
  def color_point({_, iterations}, params) do
    case params.color do
      :black_on_white -> BlackAndWhiteAndGray.black_on_white(iterations, params)
      :white_on_black -> BlackAndWhiteAndGray.white_on_black(iterations, params)
      :gray -> BlackAndWhiteAndGray.gray(iterations, params)
      :red -> WarpPov.red(iterations, params)
      :green -> WarpPov.green(iterations, params)
      :blue -> WarpPov.blue(iterations, params)
      :random -> Random.at(Random, iterations, params)
    end
  end
end
