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

  def handle_cast({:work, params}, :ok) do
    {:ok, _pid} = Task.start(fn ->
      Grid.grid(params)
      |> Grid.chunk(params)
      |> Stream.each(&send_chunk(&1))
      |> Stream.run
    end)
    {:noreply, :ok}
  end

  def send_chunk(chunk) do
    EscapeTimeWorker.escape_time(EscapeTimeWorker, chunk)
  end
end
