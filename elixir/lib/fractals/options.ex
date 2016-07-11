defmodule Fractals.Options do
  @moduledoc """
  Structure for options when generating fractals.
  """

  defstruct [
    # operational
    :seed,
    :chunk_size, :chunk_count,
    # fractal
    :fractal,
    :c, :z, :r, :p,
    # image
    :size, :color,
    :upper_left, :lower_right,
    # output
    :output_filename, :output_pid,
    # processes
    :next_pid
  ]

  import Complex, only: :macros

  alias Fractals.{Options, Size}

  # NOTE: does not work as module variable because of compiler deadlock
  # cf. https://github.com/elixir-lang/elixir/issues/4844
  defp default_options do
    %Options{
      seed:       666,
      chunk_size: 1000,
      fractal: :mandelbrot,
      p:       Complex.zero,
      r:       Complex.zero,
      z:       Complex.zero,
      c:       cmplx(1.0),
      size:    %Size{width: 512, height: 384},
      color:   :black_on_white,
      upper_left:  cmplx(5.0, 6.0),
      lower_right: cmplx(6.0, 5.0)
    }
  end

  def parse(raw_params, options \\ default_options) do
    raw_params
    |> Enum.reduce(options, &accumulate_attribute/2)
    |> precompute
  end

  def close(options) do
    :ok = File.close(options.output_pid)
    options
  end

  defp precompute(options) do
    options
    |> Map.put(:chunk_count, compute_chunk_count(options))
  end

  defp accumulate_attribute({attribute, value}, options) do
    parse_attribute(attribute, value, options)
  end

  defp parse_attribute(:params_filename, filename, options) do
    yaml = filename |> YamlElixir.read_from_file |> symbolize
    parse(yaml, options)
  end
  defp parse_attribute(:output_filename, filename, options) do
    %{options | output_pid: File.open!(filename, [:write])}
  end
  defp parse_attribute(attribute, value, options) do
    %{options | attribute => parse_value(attribute, value)}
  end

  defp parse_value(:fractal, value) do
    String.to_atom(String.downcase(value))
  end
  defp parse_value(:c, value) do
    Complex.parse(value)
  end
  defp parse_value(:color, color) do
    String.to_atom(Inflex.underscore(color))
  end
  defp parse_value(:upper_left, value) do
    Complex.parse(value)
  end
  defp parse_value(:lower_right, value) do
    Complex.parse(value)
  end
  defp parse_value(:p, value) do
    Complex.parse(value)
  end
  defp parse_value(:r, value) do
    Complex.parse(value)
  end
  defp parse_value(:z, value) do
    Complex.parse(value)
  end
  defp parse_value(:size, value) do
    [_, width, height] = Regex.run(~r/(\d+)x(\d+)/, value)
    %Size{
     width:  String.to_integer(width),
     height: String.to_integer(height)
    }
  end
  defp parse_value(_attribute, value), do: value

  defp symbolize(params) do
    for {key, val} <- params, into: %{}, do: {to_atom(key), val}
  end

  defp to_atom(key) when is_atom(key), do: key
  defp to_atom(key), do: String.to_atom(Macro.underscore(key))

  defp compute_chunk_count(%Options{size: %Size{width: width, height: height}, chunk_size: chunk_size}) do
    pixel_count = width * height
    if rem(pixel_count, chunk_size) == 0 do
      div(pixel_count, chunk_size)
    else
      div(pixel_count, chunk_size) + 1
    end
  end
end
