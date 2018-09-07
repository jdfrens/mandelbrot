defmodule Fractals.ImageMagick do
  @moduledoc """
  Calls `convert` in a shell to convert file formats.
  """

  @spec convert(String.t(), String.t()) :: Mogrify.Image.t()
  def convert(input_filename, output_filename) do
    input_filename
    |> Mogrify.open()
    |> Mogrify.format("png")
    |> Mogrify.save(path: output_filename)
  end
end
