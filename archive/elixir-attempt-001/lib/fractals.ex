defmodule Fractals do
  @moduledoc """
  Provides the main routine for the CLI app.
  """

  def main(argv) do
    argv
    |> OptionParser.parse
    |> take_action
  end

  def take_action({flags, [ options_filename, output_filename ], _}) do
    options = Fractals.Options.parse(flags, options_filename, output_filename)
    if options.fractal == :nova do
      IO.puts "Nova fractal not supported."
    else
      elapsed_time = generate(options)
      IO.puts "ran in #{elapsed_time} seconds"
    end
  end

  def generate(options) do
    measure(fn ->
      {:ok, image_file} = File.open(options.output_filename, [:write])

      options
      |> Fractals.Generator.generate
      |> Stream.each(fn line -> IO.puts(image_file, line) end)
      |> Stream.run

      File.close(image_file)
    end)
  end


  def measure(function) do
    function
    |> :timer.tc
    |> elem(0)
    |> Kernel./(1_000_000)
  end
end
