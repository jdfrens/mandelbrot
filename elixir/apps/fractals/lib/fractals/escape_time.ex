defmodule Fractals.EscapeTime do
  @moduledoc """
  Implements the basic escape-time algorithm for fractals.
  """

  @type t :: [Complex.complex()]

  import Fractals.EscapeTime.Helpers
  alias Fractals.{EscapeTime, Params}
  alias Fractals.EscapeTime.{BurningShip, Julia, Mandelbrot}

  defmacro __using__(_options) do
    quote do
      @spec pixels([Complex.complex()], Params.t()) :: Fractals.EscapeTime.t()
      def pixels(grid_points, params) do
        Enum.map(grid_points, fn grid_point ->
          grid_point
          |> iterate(params)
          |> EscapeTime.escape_time(params)
        end)
      end
    end
  end

  @spec escape_time(Enumerable.t(), Params.t()) :: Complex.complex()
  def escape_time(stream, params) do
    stream
    |> Stream.with_index()
    |> Stream.drop_while(fn zi -> !done?(zi, params) end)
    |> Stream.take(1)
    |> Enum.to_list()
    |> List.first()
  end

  @spec pixels(Params.fractal_type(), list, Params.t()) :: t()
  def pixels(:mandelbrot, data, params) do
    Mandelbrot.pixels(data, params)
  end

  def pixels(:julia, data, params) do
    Julia.pixels(data, params)
  end

  def pixels(:burningship, data, params) do
    BurningShip.pixels(data, params)
  end
end
