defmodule Fractals.Params do
  @moduledoc """
  Structure for params when generating fractals.
  """

  defstruct [
    # operational
    :id,
    :seed,  # TODO: actually use the seed
    :chunk_size, :chunk_count,
    :max_iterations, :cutoff_squared,
    # fractal
    :fractal,
    :c, :z, :r, :p,
    # image
    :size, :color,
    :upper_left, :lower_right,
    :max_intensity,
    # input
    :params_filename,
    # output
    :output_directory, :output_filename, :ppm_filename, :output_pid,
    # processes
    :source_pid
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
      lower_right:    cmplx(6.0, 5.0),
      output_directory: "images"
    }
  end

  @computed_attributes [:id, :chunk_count, :output_filename, :ppm_filename, :output_pid]
  @complex_attributes [:upper_left, :lower_right, :c, :p, :r, :z]
  # IDEA: this could be a param
  @output_extension ".png"

  # IDEA: don't let user set some values (like output_pid)
  # IDEA: add `postcheck` after `compute` to check necessary values

  def process(raw_params, params \\ default()) do
    raw_params
    |> parse(params)
    |> compute
  end

  def parse(raw_params, params \\ default()) do
    raw_params
    |> Enum.reduce(params, &parse_attribute/2)
  end

  def close(params) do
    :ok = File.close(params.output_pid)
    params
  end

  # *******
  # Parsing
  # *******

  defp parse_attribute({:params_filename, filename}, params) do
    with {:ok, raw_yaml} <- YamlElixir.read_from_file(filename),
         yaml <- symbolize(raw_yaml),
         do: parse(yaml, %{params | params_filename: filename})
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

  # **********
  # Compute
  # **********

  defp compute(params) do
    Enum.reduce(@computed_attributes, params, &compute_attribute/2)
  end

  defp compute_attribute(attribute, params) do
    %{params | attribute => compute_value(attribute, params)}
  end

  defp compute_value(:id, _params) do
    UUID.uuid1
  end
  defp compute_value(:chunk_count, params) do
    pixel_count = params.size.width * params.size.height
    if rem(pixel_count, params.chunk_size) == 0 do
      div(pixel_count, params.chunk_size)
    else
      div(pixel_count, params.chunk_size) + 1
    end
  end
  defp compute_value(:output_filename, params) do
    case params.params_filename do
      nil ->
        nil
      filename ->
        output_basepath(filename, params) <> @output_extension
    end
  end
  defp compute_value(:ppm_filename, params) do
    case params.output_filename do
      nil ->
        nil
      filename ->
        output_basepath(filename, params) <> ".ppm"
    end
  end
  defp compute_value(:output_pid, params) do
    case params.ppm_filename do
      nil ->
        nil
      filename ->
        File.open!(filename, [:write])
    end
  end

  defp output_basepath(filename, params) do
    Path.join(params.output_directory, basename(filename))
  end

  defp basename(filename) do
    filename
    |> Path.basename(".yml")
    |> Path.basename(".png")
    |> Path.basename(".ppm")
  end

  # *******
  # Helpers
  # *******

  defp symbolize(params) do
    for {key, val} <- params, into: %{}, do: {to_atom(key), val}
  end

  defp to_atom(key) when is_atom(key), do: key
  defp to_atom(key), do: String.to_atom(Macro.underscore(key))
end
