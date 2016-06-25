defmodule Mix.Tasks.Fractals.Generate do
  use Mix.Task

  @shortdoc "Generate the fractals!"
  def run(_args) do
    Mix.Task.run "escript.build"
    Enum.each(input_filenames, &generate/1)
  end

  def generate(input_filename) do
    ppm_filename = input_to_image_filename(input_filename, ".ppm")
    png_filename = input_to_image_filename(input_filename, ".png")
    IO.puts "Generating #{ppm_filename}"
    Mix.shell.cmd("fractals #{input_filename} #{ppm_filename}")
    IO.puts "Generating #{png_filename}"
    Mix.shell.cmd("convert #{ppm_filename} #{png_filename}")
  end

  def input_filenames do
    Path.wildcard("../json/mandelbrot*.json")
  end

  def input_to_image_filename(input_filename, extension) do
    Path.join(["images", Path.basename(input_filename, ".json") <> extension])
  end
end
