defmodule Fractals do
  @moduledoc """
  Provides the main routine for the CLI app.
  """

  def main(argv) do
    argv
    |> OptionParser.parse
    |> take_action
  end

  def take_action({ _, [ options_filename, image_filename ], _ }) do
    {:ok, image_file} = File.open(image_filename, [:write])

    options_filename
    |> options
    |> Fractals.Generator.generate
    |> Stream.each(fn line -> IO.puts(image_file, line) end)
    |> Stream.run

    File.close(image_file)
  end

  def options(options_filename) do
    options_filename |> File.read! |> Fractals.Options.parse()
  end

end
