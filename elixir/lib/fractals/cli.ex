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

    filenames
    |> Enum.each(fn params_filename ->
      []
      |> Keyword.put(:params_filename, params_filename)
      |> Keyword.put(:source_pid, self())
      |> Params.process(base_params)
      |> Fractals.fractalize()
    end)

    Fractals.Reports.FilenameCountdown.start_link(filenames: filenames, for: self())
    Fractals.Reports.Stdout.start_link([])
    watch(filenames)
  end

  @spec watch([String.t()]) :: :ok
  def watch([]) do
    receive do
      {:filenames_empty, _reason} ->
        IO.puts("I GOT THE MESSAGE!!!!")
    after
      5000 ->
        IO.puts("NOOOOOO MESSAGE!!!!!")
    end

    IO.puts("ALL DONE!")
    IO.puts("Have a nice day.")
    :ok
  end

  def watch(filenames) do
    receive do
      {:starting, params, opts} ->
        Fractals.Reports.Stdout.report(Fractals.Reports.Stdout, {:starting, params, opts})

        Fractals.Reports.FilenameCountdown.report(
          Fractals.Reports.FilenameCountdown,
          {:starting, params, opts}
        )

        watch(filenames)

      {:writing, params, opts} ->
        Fractals.Reports.Stdout.report(Fractals.Reports.Stdout, {:writing, params, opts})

        Fractals.Reports.FilenameCountdown.report(
          Fractals.Reports.FilenameCountdown,
          {:writing, params, opts}
        )

        watch(filenames)

      {:skipping, params, opts} ->
        Fractals.Reports.Stdout.report(Fractals.Reports.Stdout, {:skipping, params, opts})

        Fractals.Reports.FilenameCountdown.report(
          Fractals.Reports.FilenameCountdown,
          {:skipping, params, opts}
        )

        watch(List.delete(filenames, params.params_filename))

      {:done, params, opts} ->
        Fractals.Reports.Stdout.report(Fractals.Reports.Stdout, {:done, params, opts})

        Fractals.Reports.FilenameCountdown.report(
          Fractals.Reports.FilenameCountdown,
          {:done, params, opts}
        )

        watch(List.delete(filenames, params.params_filename))
    end
  end
end
