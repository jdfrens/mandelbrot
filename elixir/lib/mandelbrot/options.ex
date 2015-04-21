defmodule Mandelbrot.Options do
  defstruct [:fractal]

  def parse(json_str) do
    options = Poison.decode!(json_str, as: Mandelbrot.Options)
    %{ options | fractal: String.to_atom(String.downcase(options.fractal)) }
  end

end
