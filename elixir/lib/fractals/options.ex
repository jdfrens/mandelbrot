defmodule Fractals.Options do
  @moduledoc """
  Structure for options when generating fractals.

  `parse` will parse a JSON string.
  """

  alias Fractals.{Options, Size}

  defstruct [
    # from input file
    :fractal, :size, :color, :seed,
    :upper_left, :lower_right,
    :c, :z, :r, :p,
    # output
    :output_filename,
    :output_pid,
    # chunking
    :chunk_count,
    :chunk_size,
    # pid to send updates
    :next_pid,
  ]

  @default_flags [chunk_size: 1000]

  def merge_configs(configs) do
    configs
    |> Enum.map(&Enum.into(&1, %{}))
    |> Enum.reduce(&Map.merge(&2, &1))
  end

  def parse(flags, options_filename, image_filename) do
    %Options{output_filename: image_filename}
    |> parse_file(options_filename)
    |> parse_flags(flags)
    |> precompute
  end

  def open_output_file(options) do
    Map.put(options, :output_pid, File.open!(options.output_filename, [:write]))
  end

  def close_output_file(options) do
    :ok = File.close(options.output_pid)
    options
  end

  def set_next_pid(options, pid) do
    Map.put(options, :next_pid, pid)
  end

  def precompute(options) do
    options
    |> Map.put(:chunk_count, compute_chunk_count(options))
  end

  def compute_chunk_count(%Options{size: %Size{width: width, height: height}, chunk_size: chunk_size}) do
    pixel_count = width * height
    if rem(pixel_count, chunk_size) == 0 do
      div(pixel_count, chunk_size)
    else
      div(pixel_count, chunk_size) + 1
    end
  end

  def parse_file(options, filename) do
    yaml = YamlElixir.read_from_file(filename, atoms: true)
    parse(options, yaml)
  end

  def parse(options, json) do
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
      chunk_size:  Keyword.fetch!(flags, :chunk_size)
      }
  end

  defp parse_fractal(fractal) do
    String.to_atom(String.downcase(fractal))
  end

  defp parse_size(nil), do: %Size{width: 512, height: 384}
  defp parse_size(size) do
    [_, width, height] = Regex.run(~r/(\d+)x(\d+)/, size)
    %Size{
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
