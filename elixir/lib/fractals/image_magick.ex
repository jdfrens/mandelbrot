defmodule Fractals.ImageMagick do
  @moduledoc """
  Calls `convert` in a shell to convert file formats.
  """

  def convert(input_filename, output_filename) do
    %Porcelain.Result{out: _output, status: _status} =
      Porcelain.shell("convert #{input_filename} #{output_filename}")
  end
end
