defmodule Fractals.GridWorker do
  use GenServer

  alias Fractals.Grid
  alias Fractals.EscapeTimeWorker

  # Client

  def start_link(params) do
    GenServer.start_link(__MODULE__, params, name: __MODULE__)
  end

  def work(pid) do
    GenServer.cast(pid, :work)
  end

  # Server

  def handle_cast(:work, options) do
    {:ok, _pid} = Task.start(fn ->
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
