defmodule Fractals.ImageMagick do
  def convert(input_filename, output_filename) do
    %Porcelain.Result{out: _output, status: _status} =
      Porcelain.shell("convert #{input_filename} #{output_filename}")
  end
end
