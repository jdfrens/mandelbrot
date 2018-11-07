defmodule Fractals.Output.PPMFile do
  def start_file(params) do
    lines_to_file(header(params), params)
  end

  def write_file(params, pixels) do
    lines_to_file(header(params), params)
    lines_to_file(pixels, params)
  end

  @spec header(Params.t()) :: [String.t()]
  def header(params) do
    PPM.p3_header(params.size.width, params.size.height)
  end

  @spec lines_to_file([String.t()], Params.t()) :: :ok
  def lines_to_file(lines, params) do
    IO.write(params.output_pid, add_newlines(lines))
  end

  @spec add_newlines([String.t()]) :: [[String.t()]]
  def add_newlines(lines) do
    Enum.map(lines, &[&1, "\n"])
  end
end
