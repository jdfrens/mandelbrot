defmodule Fractals.GridWorker do
  use GenServer

  alias Fractals.Grid
  alias Fractals.EscapeTimeWorker

  # Client

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def work(pid, params) do
    GenServer.cast(pid, {:work, params})
  end

  # Server

  def init(:ok) do
    {:ok, :state}
  end

  def handle_cast({:work, params}, :state) do
    Grid.grid(params)
    |> Grid.chunk(params)
    |> Stream.each(&send_chunk(&1))
    |> Stream.run
    {:noreply, :ok}
  end

  def send_chunk(chunk) do
    Progress.incr(:generate_chunk)
    EscapeTimeWorker.escape_time(EscapeTimeWorker, chunk)
  end
end
