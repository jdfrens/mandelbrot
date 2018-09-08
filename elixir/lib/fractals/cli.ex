defmodule Fractals.CLI do
  @moduledoc """
  The command-line interface for generating fractals.  This also starts up the application itself.
  """

  alias Fractals.Params

  @spec main(OptionParser.argv()) :: :ok
  def main(args) do
    {flags, filenames, _} = OptionParser.parse(args, switches: [params_filename: :string])
    go(flags, filenames)
  end

  @spec go(OptionParser.parsed(), OptionParser.argv()) :: :ok
  def go(flags, filenames) do
    base_params = flags |> Params.parse()

    Fractals.Reporters.FilenameCountdown.start_link(filenames: filenames, for: self())
    Fractals.Reporters.Stdout.start_link([])

    filenames
    |> Enum.each(fn params_filename ->
      []
      |> Keyword.put(:params_filename, params_filename)
      |> Keyword.put(:source_pid, self())
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
