defmodule Mix.Tasks.Fractals.Generate do
  use Mix.Task

  @default_fractals ["mandelbrot*", "julia*", "burningship*"]

  @shortdoc "Generate the fractals!"
  def run(args) do
    Mix.Task.run "escript.build"
    case args do
      [] ->
        run(@default_fractals)
      _ ->
        Enum.each(input_filenames(args), &generate/1)
    end
  end

  def generate(input_filename) do
    ppm_filename = input_to_image_filename(input_filename, ".ppm")
    png_filename = input_to_image_filename(input_filename, ".png")
    IO.puts "Generating #{ppm_filename}"
    Mix.shell.cmd("fractals #{input_filename} #{ppm_filename}")
    IO.puts "Generating #{png_filename}"
    Task.start(fn ->
      Mix.shell.cmd("convert #{ppm_filename} #{png_filename}")
    end)
  end

  def input_filenames(base_names) do
    base_names
    |> Enum.map(&("../yaml/#{&1}.yml"))
    |> Enum.flat_map(&Path.wildcard/1)
  end

  def input_to_image_filename(input_filename, extension) do
    Path.join(["images", Path.basename(input_filename, ".yml") <> extension])
  end
end
