defmodule CLI do
  @moduledoc """
  The command-line interface for generating fractals.  This also starts up the application itself.
  """

  # eventually determined in Params
  @engine StageEngine

  alias Fractals.Params
  alias Fractals.Reporters.{Broadcaster, FilenameCountdown, Stdout}

  @spec main(OptionParser.argv()) :: :ok
  def main(args) do
    {flags, filenames, _} = OptionParser.parse(args, switches: [params_filename: :string])
    go(flags, filenames)
  end

  @spec go(OptionParser.parsed(), OptionParser.argv()) :: :ok
  def go(flags, filenames) do
    add_reporters(filenames)
    spawn_fractals(flags, filenames)
    wait()
  end

  @spec spawn_fractals(keyword, [String.t()]) :: :ok
  defp spawn_fractals(flags, filenames) do
    filenames
    |> Enum.each(fn params_filename ->
      []
      |> Keyword.put(:params_filename, params_filename)
      |> Params.process(Params.parse(flags))
      |> Fractals.fractalize(@engine)
    end)

    :ok
  end

  @spec add_reporters([String.t()]) :: :ok
  def add_reporters(filenames) do
    Broadcaster.add_reporter(FilenameCountdown, filenames: filenames, for: self())
    Broadcaster.add_reporter(Stdout)
    :ok
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
