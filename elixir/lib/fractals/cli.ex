defmodule Fractals.CLI do
  alias Fractals.Params

  def main(args) do
    case OptionParser.parse(args) do
      {flags, [params_filename, output_filename], _} ->
        flags
        |> Keyword.put(:params_filename, params_filename)
        |> Keyword.put(:output_filename, output_filename)
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
      {:ok, _pid} = Fractals.JobSupervisor.start_child
      Fractals.GridWorker.work(Fractals.GridWorker, params)
      waiting_loop(params)
    end)
    IO.puts "#{time / 1_000_000} seconds"
  end

  def waiting_loop(params) do
    receive do
      {:writing, chunk_number} ->
        IO.puts "writing #{chunk_number} of #{params.chunk_count}"
        waiting_loop(params)
      # IDEA: could match on pid for Fractals.OutputWorker
      {:done, _} ->
        Params.close(params)
    end
  end

  defp usage do
    IO.puts "You used this command wrong."
  end
end
