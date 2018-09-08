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

    watch(filenames)
  end

  @spec watch([String.t()]) :: :ok
  def watch([]) do
    IO.puts("ALL DONE!")
    IO.puts("Have a nice day.")
    :ok
  end

  def watch(filenames) do
    receive do
      {:starting, params, _opts} ->
        IO.puts("starting #{params.output_filename}")
        watch(filenames)

      {:writing, params, opts} ->
        chunk_number = Keyword.get(opts, :chunk_number)
        IO.puts("writing #{chunk_number}/#{params.chunk_count} to #{params.ppm_filename}")
        watch(filenames)

      {:skipping, params, opts} ->
        reason = Keyword.get(opts, :reason)
        IO.puts("skipping #{params.output_filename}: #{reason}")
        watch(List.delete(filenames, params.params_filename))

      {:done, params, _opts} ->
        IO.puts("finished #{params.output_filename}")
        watch(List.delete(filenames, params.params_filename))
    end
  end
end
