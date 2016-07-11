defmodule Fractals.CLI do
  alias Fractals.Options

  def main(args) do
    case OptionParser.parse(args) do
      {flags, [params_filename, output_filename], _} ->
        flags
        |> Keyword.put(:params_filename, params_filename)
        |> Keyword.put(:output_filename, output_filename)
        |> Keyword.put(:next_pid, self)
        |> Options.parse
        |> main_helper
      _ ->
        usage()
    end
  end

  def main_helper(%Options{fractal: :nova}) do
    IO.puts "Nova fractal not supported."
  end
  def main_helper(options) do
    {time, _} = :timer.tc(fn ->
      {:ok, _pid} = Fractals.JobSupervisor.start_child(options)
      Fractals.GridWorker.work(Fractals.GridWorker)
      waiting_loop(options)
    end)
    IO.puts "#{time / 1_000_000} seconds"
  end

  def waiting_loop(options) do
    receive do
      {:writing, chunk_number} ->
        IO.puts "writing #{chunk_number} of #{options.chunk_count}"
        waiting_loop(options)
      # IDEA: could match on pid for Fractals.OutputWorker
      {:done, _} ->
        Options.close(options)
    end
  end

  defp usage do
    IO.puts "You used this command wrong."
  end
end
