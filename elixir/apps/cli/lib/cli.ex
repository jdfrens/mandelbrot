defmodule CLI do
  @moduledoc """
  The command-line interface for generating fractals.  This also starts up the application itself.
  """

  # eventually determined in Params
  @engine StageEngine
  # @engine UniprocessEngine

  alias Fractals.Params
  alias Fractals.Reporters.{Broadcaster, Countdown, Stdout}

  @spec main(OptionParser.argv()) :: :ok
  def main(args) do
    {flags, filenames, _} = OptionParser.parse(args, switches: [params_filename: :string])
    go(flags, filenames)
  end

  @spec go(OptionParser.parsed(), OptionParser.argv()) :: :ok
  def go(flags, filenames) do
    spawn_fractals(flags, filenames)
    wait()
  end

  @spec spawn_fractals(keyword, [String.t()]) :: :ok
  defp spawn_fractals(flags, filenames) do
    filenames
    |> parse_params_files(flags)
    |> add_reporters()
    |> fractalize()

    :ok
  end

  @spec parse_params_files([String.t()], keyword) :: [Params.t()]
  defp parse_params_files(filenames, flags) do
    Enum.map(filenames, fn filename ->
      Params.process([params_filename: filename], Params.parse(flags))
    end)
  end

  @spec add_reporters([Params.t()]) :: [Params.t()]
  defp add_reporters(params_list) do
    Broadcaster.add_reporter(Stdout)
    Broadcaster.add_reporter(Countdown, %{params_list: params_list, for: self()})
    params_list
  end

  @spec fractalize([Params.t()]) :: [Params.t()]
  defp fractalize(params_list) do
    Enum.each(params_list, &Fractals.fractalize(&1, @engine))
    params_list
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
