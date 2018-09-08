defmodule Fractals.Reports.Stdout do
  @moduledoc """
  Outputs messages to stdout.
  """

  use GenServer

  @type tag :: :starting | :writing | :skipping | :done

  # client

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @spec report(module | pid, {atom, Params.t(), keyword}) :: :ok
  def report(pid, message) do
    GenServer.cast(pid, message)
  end

  # server

  def init(:ok) do
    {:ok, :ok}
  end

  def handle_cast({:starting, params, _opts}, :ok) do
    IO.puts("starting #{params.output_filename}")
    {:noreply, :ok}
  end

  def handle_cast({:writing, params, opts}, :ok) do
    chunk_number = Keyword.get(opts, :chunk_number)
    IO.puts("writing #{chunk_number}/#{params.chunk_count} to #{params.ppm_filename}")
    {:noreply, :ok}
  end

  def handle_cast({:skipping, params, opts}, :ok) do
    reason = Keyword.get(opts, :reason)
    IO.puts("skipping #{params.output_filename}: #{reason}")
    {:noreply, :ok}
  end

  def handle_cast({:done, params, _opts}, :ok) do
    IO.puts("finished #{params.output_filename}")
    {:noreply, :ok}
  end
end
