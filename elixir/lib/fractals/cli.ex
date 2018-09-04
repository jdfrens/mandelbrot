defmodule Fractals.CLI do
  @moduledoc """
  The command-line interface for generating fractals.  This also starts up the application itself.
  """

  alias Fractals.Params

  def main(args) do
    {flags, filenames, _} = OptionParser.parse(args)
    go(flags, filenames)
  end

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

  def watch([]) do
    IO.puts("ALL DONE!")
    IO.puts("Have a nice day.")
  end

  def watch(filenames) do
    receive do
      {:starting, _from, params} ->
        IO.puts("starting #{params.output_filename}")
        watch(filenames)

      {:writing, chunk_number, params} ->
        IO.puts("writing #{chunk_number}/#{params.chunk_count} to #{params.ppm_filename}")
        watch(filenames)

      {:skipping, _, params, reason} ->
        IO.puts("skipping #{params.output_filename}: #{reason}")
        watch(List.delete(filenames, params.params_filename))

      {:done, _, params} ->
        IO.puts("finished #{params.output_filename}")
        watch(List.delete(filenames, params.params_filename))
    end
  end
end
