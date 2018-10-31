defmodule Fractals.Params do
  @moduledoc """
  Structure for params when generating fractals.
  """

  import Complex, only: :macros

  alias Fractals.{Params, Size}

  @type fractal_id :: String.t()
  @type fractal_type :: :mandelbrot | :julia
  @type color :: :black_on_white | :white_on_black | :gray | :red | :green | :blue | :random
  @type t :: %__MODULE__{
          id: fractal_id() | nil,
          seed: integer | nil,
          chunk_size: integer | nil,
          chunk_count: integer | nil,
          max_iterations: integer | nil,
          cutoff_squared: float | nil,
          fractal: fractal_type | nil,
          c: Complex.complex() | nil,
          z: Complex.complex() | nil,
          r: Complex.complex() | nil,
          p: Complex.complex() | nil,
          size: Size.t() | nil,
          color: color() | nil,
          upper_left: Complex.complex() | nil,
          lower_right: Complex.complex() | nil,
          max_intensity: integer | nil,
          params_filename: String.t() | nil,
          output_directory: String.t() | nil,
          output_filename: String.t() | nil,
          ppm_filename: String.t() | nil,
          output_pid: pid | nil
        }

  defstruct [
    # operational
    :id,
    :seed,
    :chunk_size,
    :chunk_count,
    :max_iterations,
    :cutoff_squared,
    # fractal
    :fractal,
    :c,
    :z,
    :r,
    :p,
    # image
    :size,
    :color,
    :upper_left,
    :lower_right,
    :max_intensity,
    # input
    :params_filename,
    # output
    :output_directory,
    :output_filename,
    :ppm_filename,
    :output_pid
  ]

  @zero Complex.new(0.0)
  @one Complex.new(1.0)

  @spec default :: Params.t()
  def default do
    %Params{
      seed: 666,
      chunk_size: 1000,
      cutoff_squared: 4.0,
      max_iterations: 256,
      fractal: :mandelbrot,
      p: @zero,
      r: @zero,
      z: @zero,
      c: @one,
      size: %Size{width: 512, height: 384},
      color: :black_on_white,
      max_intensity: 255,
      upper_left: Complex.new(5.0, 6.0),
      lower_right: Complex.new(6.0, 5.0),
      output_directory: "images"
    }
  end

  @computed_attributes [:id, :chunk_count, :output_filename, :ppm_filename, :output_pid]
  @complex_attributes [:upper_left, :lower_right, :c, :p, :r, :z]
  # IDEA: this could be a param
  @output_extension ".png"

  # IDEA: don't let user set some values (like output_pid)
  # IDEA: add `postcheck` after `compute` to check necessary values

  @spec process(map | keyword, Params.t()) :: Params.t()
  def process(raw_params, params \\ default()) do
    raw_params
    |> parse(params)
    |> compute
  end

  @spec parse(map | keyword, Params.t()) :: Params.t()
  def parse(raw_params, params \\ default()) do
    raw_params
    |> Enum.reduce(params, &parse_attribute/2)
  end

  @spec close(Params.t()) :: Params.t()
  def close(params) do
    :ok = File.close(params.output_pid)
    params
  end

  # *******
  # Parsing
  # *******

  @spec parse_attribute({atom, any}, Params.t()) :: Params.t()
  defp parse_attribute({:params_filename, filename}, params) do
    with {:ok, raw_yaml} <- YamlElixir.read_from_file(filename),
         yaml <- symbolize(raw_yaml),
         do: parse(yaml, %{params | params_filename: filename})
  end

  defp parse_attribute({attribute, value}, params) do
    %{params | attribute => parse_value(attribute, value)}
  end

  @spec parse_value(atom, String.t()) :: any
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
      width: String.to_integer(width),
      height: String.to_integer(height)
    }
  end

  defp parse_value(_attribute, value), do: value

  # **********
  # Compute
  # **********

  @spec compute(Params.t()) :: Params.t()
  defp compute(params) do
    Enum.reduce(@computed_attributes, params, &compute_attribute/2)
  end

  @spec compute_attribute(atom, Params.t()) :: Params.t()
  defp compute_attribute(attribute, params) do
    %{params | attribute => compute_value(attribute, params)}
  end

  @spec compute_value(atom, Params.t()) :: any
  defp compute_value(:id, _params) do
    UUID.uuid1()
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

  @spec output_basepath(String.t(), Params.t()) :: String.t()
  defp output_basepath(filename, params) do
    Path.join(params.output_directory, basename(filename))
  end

  @spec basename(String.t()) :: String.t()
  defp basename(filename) do
    filename
    |> Path.basename(".yml")
    |> Path.basename(".png")
    |> Path.basename(".ppm")
  end

  # *******
  # Helpers
  # *******

  @spec symbolize(Enumerable.t()) :: map
  defp symbolize(params) do
    for {key, val} <- params, into: %{}, do: {to_atom(key), val}
  end

  @spec to_atom(atom | String.t()) :: atom
  defp to_atom(key) when is_atom(key), do: key
  defp to_atom(key), do: String.to_atom(Macro.underscore(key))
end
