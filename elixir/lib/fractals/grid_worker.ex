defmodule Fractals.GridWorker do
  use GenServer

  alias Fractals.Grid
  alias Fractals.EscapeTimeWorker

  # Client

  def start_link([options]) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  def work(pid) do
    GenServer.cast(pid, :work)
  end

  # Server

  def init(options) do
    {:ok, options}
  end

  def handle_cast(:work, options) do
    Task.start(fn ->
      Grid.grid(options)
      |> Grid.chunk(options)
      |> Stream.each(&send_chunk(&1))
      |> Stream.run
    end)
    {:noreply, options}
  end

  def send_chunk(chunk) do
    EscapeTimeWorker.escape_time(EscapeTimeWorker, chunk)
  end
end
