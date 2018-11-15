defmodule Fractals.EscapeTime.Helpers do
  @moduledoc """
  Macros and functions for determining properties of a point in the fractal during an escape-time algorithm.
  """

  defmacro inside?(iterations, max_iterations) do
    quote do
      unquote(iterations) >= unquote(max_iterations)
    end
  end

  defmacro outside?(z, cutoff_squared) do
    quote do
      Complex.abs_squared(unquote(z)) >= unquote(cutoff_squared)
    end
  end

  @spec done?({Complex.complex(), non_neg_integer}, Fractals.Params.t()) :: boolean
  def done?({z, iterations}, params) do
    outside?(z, params.cutoff_squared) || inside?(iterations, params.max_iterations)
  end
end
