defmodule Fractals.CLI do
  @moduledoc """
  The command-line interface for generating fractals.  This also starts up the application itself.
  """

  alias Fractals.Params
  alias Fractals.Reporters.{FilenameCountdown, Stdout}

  @spec main(OptionParser.argv()) :: :ok
  def main(args) do
    {flags, filenames, _} = OptionParser.parse(args, switches: [params_filename: :string])
    go(flags, filenames)
  end

  @spec go(OptionParser.parsed(), OptionParser.argv()) :: :ok
  def go(flags, filenames) do
    base_params = flags |> Params.parse()

    FilenameCountdown.start_link(filenames: filenames, for: self())
    Stdout.start_link([])

    filenames
    |> Enum.each(fn params_filename ->
      []
      |> Keyword.put(:params_filename, params_filename)
      |> Params.process(base_params)
      |> Fractals.fractalize()
    end)

    wait()
  end

  @spec wait :: :ok
  def wait do
    receive do
      {:filenames_empty, _reason} ->
        IO.puts("ALL DONE!")
        IO.puts("Have a nice day.")
    end

    :ok
  end
end
