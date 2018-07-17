defmodule Fractals.Colorizer do
  @moduledoc """
  This finds the right colorizer as specified in the params.
  """

  # TODO: move black/white/gray generators.

  alias Fractals.Colorizer.{BlackAndWhiteAndGray, Random, WarpPov}

  @spec color_point({Complex.t(), non_neg_integer}, Params) :: String.t()
  def color_point({_, iterations}, params) do
    case params.color do
      :black_on_white -> BlackAndWhiteAndGray.black_on_white(iterations, params.max_iterations)
      :white_on_black -> BlackAndWhiteAndGray.white_on_black(iterations, params.max_iterations)
      :gray -> BlackAndWhiteAndGray.gray(iterations, params)
      :red -> WarpPov.red(iterations, params)
      :green -> WarpPov.green(iterations, params)
      :blue -> WarpPov.blue(iterations, params)
      :random -> Random.at(Random, iterations, params)
    end
  end
end
