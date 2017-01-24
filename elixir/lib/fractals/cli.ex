defmodule Fractals.CLI do
  alias Fractals.Params

  def main(args) do
    case OptionParser.parse(args) do
      {flags, filenames, _} ->
        go(flags, filenames)
      _ ->
        usage()
    end
  end

  def go(flags, filenames) do
    filenames
    |> Enum.each(fn params_filename ->
      flags
      |> Keyword.put(:params_filename, params_filename)
      |> Keyword.put(:output_filename, output_filename(params_filename))
      |> Keyword.put(:source_pid, self())
      |> Params.parse
      |> Fractals.fractalize
    end)
    loop(filenames)
  end

  def loop([]) do
    IO.puts "ALL DONE!"
    IO.puts "Have a nice day."
  end
  def loop(filenames) do
    receive do
      {:writing, chunk_number, params} ->
        IO.puts "writing #{chunk_number}/#{params.chunk_count} to #{params.ppm_filename}"
        loop(filenames)
      {:done, _, params} ->
        loop(List.delete(filenames, List.last(params.params_filenames)))
    end
  end

  defp output_filename(params_filename) do
    output_basename =
      params_filename
      |> Path.basename
      |> Path.rootname(".yml")
    Path.join("images", output_basename <> ".png")
  end

  defp usage do
    IO.puts "You used this command wrong."
  end
end
