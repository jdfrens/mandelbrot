defmodule Fractals.EscapeTime.Helpers do
  defmacro inside?(iterations, max_iterations) do
    quote do
      unquote(iterations) >= unquote(max_iterations)
    end
  end

  defmacro escaped?(z, cutoff_squared) do
    quote do
      Complex.magnitude_squared(unquote(z)) >= unquote(cutoff_squared)
    end
  end

  def done?({z, iterations}, params) do
    escaped?(z, params.cutoff_squared) ||
      inside?(iterations, params.max_iterations)
  end
end
