defmodule Mandelbrot do
  def main(argv) do
    argv
    |> OptionParser.parse()
    |> take_action
  end

  def take_action({ _, [ json_filename, ppm_filename ], _ }) do
    read!(json_filename)
    |> Mandelbrot.Options.parse()
    |> Mandelbrot.Fractal.generate()
    |> write!(ppm_filename)
  end

  def read!(filename) do
    File.read!(filename)
  end


  def write!(ppm, ppm_filename) do
    File.write!(ppm_filename, ppm)
  end

end
