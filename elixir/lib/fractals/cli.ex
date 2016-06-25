defmodule Fractals.CLI do
  alias Fractals.Options

  def main(args) do
    case OptionParser.parse(args) do
      {flags, [options_filename, output_filename], _} ->
        main_helper(Options.parse(flags, options_filename, output_filename))
      _ ->
        usage()
    end
  end

  def main_helper(%Options{fractal: :nova}) do
    IO.puts "Nova fractal not supported."
  end
  def main_helper(%Options{} = options) do
    # SMELL: this is probably too much for this module, but where to move it?
    # YUCK: this should be done elsewhere, I think
    options =
      options
      |> Options.open_output_file
      |> Options.set_next_pid(self)
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
        Options.close_output_file(options)
    end
  end

  defp usage do
    IO.puts "You used this command wrong."
  end
end
