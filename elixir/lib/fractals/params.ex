defmodule Fractals.Params do
  @moduledoc """
  Structure for params when generating fractals.
  """

  defstruct [
    # operational
    :seed,
    :chunk_size, :chunk_count,
    :max_iterations, :cutoff_squared,
    # fractal
    :fractal,
    :c, :z, :r, :p,
    # image
    :size, :color,
    :upper_left, :lower_right,
    :max_intensity,
    # output
    :output_filename, :output_pid,
    # processes
    :next_pid
  ]

  import Complex, only: :macros

  alias Fractals.{Params, Size}

  def default do
    %Params{
      seed:           666,
      chunk_size:     1000,
      cutoff_squared: 4.0,
      max_iterations: 256,
      fractal:        :mandelbrot,
      p:              Complex.zero,
      r:              Complex.zero,
      z:              Complex.zero,
      c:              cmplx(1.0),
      size:           %Size{width: 512, height: 384},
      color:          :black_on_white,
      max_intensity:  255,
      upper_left:     cmplx(5.0, 6.0),
      lower_right:    cmplx(6.0, 5.0)
    }
  end

  @precomputed_attributes [:chunk_count]
  @complex_attributes [:upper_left, :lower_right, :c, :p, :r, :z]

  def parse(raw_params, params \\ default) do
    raw_params
    |> Enum.reduce(params, &parse_attribute/2)
    |> precompute
  end

  def close(params) do
    :ok = File.close(params.output_pid)
    params
  end

  defp precompute(params) do
    Enum.reduce(@precomputed_attributes, params, &precompute_attribute/2)
  end

  defp precompute_attribute(attribute, params) do
    %{params | attribute => precompute_value(attribute, params)}
  end

  defp parse_attribute({:params_filename, filename}, params) do
    yaml = filename |> YamlElixir.read_from_file |> symbolize
    parse(yaml, params)
  end
  defp parse_attribute({:output_filename, filename}, params) do
    %{params | output_pid: File.open!(filename, [:write])}
  end
  defp parse_attribute({attribute, value}, params) do
    %{params | attribute => parse_value(attribute, value)}
  end

  defp parse_value(:fractal, value) do
    String.to_atom(String.downcase(value))
  end
  defp parse_value(:color, color) do
    String.to_atom(Inflex.underscore(color))
  end
  defp parse_value(attribute, value) when attribute in @complex_attributes do
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

  defp precompute_value(:chunk_count, params) do
    pixel_count = params.size.width * params.size.height
    if rem(pixel_count, params.chunk_size) == 0 do
      div(pixel_count, params.chunk_size)
    else
      div(pixel_count, params.chunk_size) + 1
    end
  end
end
