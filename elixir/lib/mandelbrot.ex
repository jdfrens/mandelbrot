defmodule Mandelbrot do
  def main(argv) do
    argv
    |> OptionParser.parse()
    |> take_action
  end

  def take_action({ _, [ options_filename, image_filename ], _ }) do
    {:ok, image_file} = File.open(image_filename, [:write])

    options(options_filename)
    |> Mandelbrot.Fractal.generate()
    |> Stream.each(fn line -> IO.puts(image_file, line) end)
    |> Stream.run

    File.close(image_file)
  end

  def options(options_filename) do
    File.read!(options_filename) |> Mandelbrot.Options.parse()
  end

end
