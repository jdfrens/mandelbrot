defmodule Fractals.Output.PPMFile do
  @moduledoc """
  Functions to write a fractal to a PPM file.
  """

  @type pixel :: String.t()
  @type pixels :: [pixel()]

  alias Fractals.Params

  @doc """
  Writes all of the `pixels` to a new file.
  """
  @spec write_file(Params.t(), pixels()) :: :ok
  def write_file(params, pixels) do
    start_file(params)
    write_pixels(params, pixels)
  end

  @doc """
  Writes the PPM header to a new file.
  """
  @spec start_file(Params.t()) :: :ok
  def start_file(params) do
    lines_to_file(params, header(params))
  end

  @doc """
  Writes pixels to file that has been started with `start_file/1`.
  """
  def write_pixels(params, pixels) do
    lines_to_file(params, pixels)
  end

  @spec header(Params.t()) :: [String.t()]
  defp header(params) do
    PPM.p3_header(params.size.width, params.size.height)
  end

  @spec lines_to_file(Params.t(), [String.t()]) :: :ok
  defp lines_to_file(params, lines) do
    IO.write(params.output_pid, add_newlines(lines))
  end

  @spec add_newlines([String.t()]) :: [[String.t()]]
  defp add_newlines(lines) do
    Enum.map(lines, &[&1, "\n"])
  end
end
