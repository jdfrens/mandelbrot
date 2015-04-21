defmodule Mandelbrot.Options do
  defstruct [
    :fractal, :size, :color, :seed,
    :upper_left, :lower_right,
    :c, :z, :r, :p
  ]

  def parse(json_str) do
    json = Poison.Parser.parse!(json_str)
    %Mandelbrot.Options{
      fractal:     parse_fractal(json["fractal"]),
      size:        parse_size(json["size"]),
      color:       parse_color(json["color"]),
      seed:        parse_seed(json["seed"]),
      upper_left:  parse_complex(json["upperLeft"]),
      lower_right: parse_complex(json["lowerRight"]),
      c:           parse_complex(json["c"]),
      z:           parse_complex(json["z"]),
      r:           parse_complex(json["r"]),
      p:           parse_complex(json["p"])
    }
  end

  defp parse_fractal(fractal) do
    String.to_atom(String.downcase(fractal))
  end

  defp parse_size(size) do
    [_, width, height] = Regex.run(~r/(\d+)x(\d+)/, size)
    [ width: String.to_integer(width), height: String.to_integer(height) ]
  end

  defp parse_color(color) do
    String.to_atom(Inflex.underscore(color))
  end

  defp parse_seed(seed), do: seed

  defp parse_complex(upper_left) do
    Complex.parse(upper_left)
  end

end
