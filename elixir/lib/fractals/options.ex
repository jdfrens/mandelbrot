defmodule Fractals.Options do
  defstruct [
    :fractal, :size, :color, :seed,
    :upper_left, :lower_right,
    :c, :z, :r, :p
  ]

  def parse(json_str) do
    json = Poison.Parser.parse!(json_str)
    %Fractals.Options{
      fractal:     parse_fractal(json["fractal"]),
      size:        parse_size(json["size"]),
      color:       parse_color(json["color"]),
      seed:        parse_seed(json["seed"]),
      upper_left:  parse_complex(json["upperLeft"]),
      lower_right: parse_complex(json["lowerRight"]),
      c:           parse_complex(json["c"], %Complex{ real: 1.0, imag: 0.0 }),
      z:           parse_complex(json["z"], %Complex{ real: 0.0, imag: 0.0 }),
      r:           parse_complex(json["r"], %Complex{ real: 0.0, imag: 0.0 }),
      p:           parse_complex(json["p"], %Complex{ real: 0.0, imag: 0.0 })
    }
  end

  defp parse_fractal(fractal) do
    String.to_atom(String.downcase(fractal))
  end

  defp parse_size(nil), do: %Fractals.Size{ width: 512, height: 384 }
  defp parse_size(size) do
    [_, width, height] = Regex.run(~r/(\d+)x(\d+)/, size)
    %Fractals.Size{
     width: String.to_integer(width),
     height: String.to_integer(height)
   }
  end

  defp parse_color(nil), do: :black_on_white
  defp parse_color(color) do
    String.to_atom(Inflex.underscore(color))
  end

  defp parse_seed(nil), do: 666
  defp parse_seed(seed), do: seed

  defp parse_complex(complex), do: parse_complex(complex, nil)
  defp parse_complex(nil, default), do: default
  defp parse_complex(complex, _) do
    Complex.parse(complex)
  end

end
