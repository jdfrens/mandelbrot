defmodule Fractals.CLI do
  alias Fractals.Params

  def main(args) do
    case OptionParser.parse(args) do
      {flags, [params_filename], _} ->
        flags
        |> Keyword.put(:params_filename, params_filename)
        |> Keyword.put(:output_filename, output_filename(params_filename))
        |> Keyword.put(:source_pid, self())
        |> Params.parse
        |> main_helper
      _ ->
        usage()
    end
  end

  def main_helper(%Params{fractal: :nova}) do
    IO.puts "Nova fractal not supported."
  end
  def main_helper(params) do
    {time, _} = :timer.tc(fn ->
      Fractals.GridWorker.work(Fractals.GridWorker, params)
      waiting_loop(params)
    end)
    IO.puts "#{time / 1_000_000} seconds"
  end

  def waiting_loop(params) do
    receive do
      {:writing, filename, chunk_number} ->
        IO.puts "writing #{chunk_number}/#{params.chunk_count} to #{filename}"
        waiting_loop(params)
      {:done, _} ->
        Params.close(params)
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
