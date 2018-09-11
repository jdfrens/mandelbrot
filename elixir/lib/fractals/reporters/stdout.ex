defmodule Fractals.Reporters.Stdout do
  @moduledoc """
  Outputs messages to stdout.
  """

  use GenServer

  # client

  @spec start_link(any) :: GenServer.on_start()
  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  # server

  @impl GenServer
  def init(:ok) do
    {:ok, :ok}
  end

  @impl GenServer
  def handle_cast({:starting, params, _opts}, :ok) do
    IO.puts("starting #{params.output_filename}")
    {:noreply, :ok}
  end

  @impl GenServer
  def handle_cast({:writing, params, opts}, :ok) do
    chunk_number = Keyword.get(opts, :chunk_number)

    if Integer.mod(chunk_number, 20) == 0 or chunk_number == params.chunk_count do
      IO.puts("writing #{chunk_number}/#{params.chunk_count} to #{params.ppm_filename}")
    end

    {:noreply, :ok}
  end

  @impl GenServer
  def handle_cast({:skipping, params, opts}, :ok) do
    reason = Keyword.get(opts, :reason)
    IO.puts("skipping #{params.output_filename}: #{reason}")
    {:noreply, :ok}
  end

  @impl GenServer
  def handle_cast({:done, params, _opts}, :ok) do
    IO.puts("finished #{params.output_filename}")
    {:noreply, :ok}
  end
end
