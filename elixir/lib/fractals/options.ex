defmodule Fractals.Options do
  @moduledoc """
  Structure for options when generating fractals.

  `parse` will parse a JSON string.
  """

  defstruct [
    # from input file
    :fractal, :size, :color, :seed,
    :upper_left, :lower_right,
    :c, :z, :r, :p,
    # output
    :output_filename,
    # cached stuff
    :color_func,
    :iterator_builder,
    # from flags
    :concurrency,
    :processes,
    :chunk_size
  ]

  @default_flags [concurrency: "none", processes: 4, chunk_size: 1000]

  # FIXME: the flow for this is so terrible
  def parse(flags, options_filename, image_filename) do
    %Fractals.Options{output_filename: image_filename}
    |> parse_file(options_filename)
    |> parse_flags(flags)
  end

  def parse_file(options, filename) do
    json = filename |> File.read! |> Poison.Parser.parse!
    build_from_json(options, json)
  end

  def build_from_json(options, json) do
    %{options |
      fractal:     parse_fractal(json["fractal"]),
      size:        parse_size(json["size"]),
      color:       parse_color(json["color"]),
      seed:        parse_seed(json["seed"]),
      upper_left:  parse_complex(json["upperLeft"]),
      lower_right: parse_complex(json["lowerRight"]),
      c:           parse_complex(json["c"], %Complex{real: 1.0, imag: 0.0}),
      z:           parse_complex(json["z"], %Complex{real: 0.0, imag: 0.0}),
      r:           parse_complex(json["r"], %Complex{real: 0.0, imag: 0.0}),
      p:           parse_complex(json["p"], %Complex{real: 0.0, imag: 0.0}),
    }
  end

  def parse_flags(options, user_flags) do
    flags = Keyword.merge(@default_flags, user_flags)
    %{options |
      concurrency: Keyword.fetch(flags, :concurrency),
      processes:   Keyword.fetch(flags, :processes),
      chunk_size:  Keyword.fetch(flags, :chunk_size)
      }
  end

  defp parse_fractal(fractal) do
    String.to_atom(String.downcase(fractal))
  end

  defp parse_size(nil), do: %Fractals.Size{width: 512, height: 384}
  defp parse_size(size) do
    [_, width, height] = Regex.run(~r/(\d+)x(\d+)/, size)
    %Fractals.Size{
     width:  String.to_integer(width),
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
