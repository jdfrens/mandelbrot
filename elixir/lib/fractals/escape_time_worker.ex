defmodule Fractals.EscapeTimeWorker do
  use GenServer

  alias Fractals.ColorizerWorker
  alias Fractals.EscapeTime.Mandelbrot

  # Client

  def start_link([options]) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  def escape_time(pid, chunk) do
    GenServer.cast(pid, {:escape_time, chunk})
  end

  # Server

  def init(options) do
    {:ok, options}
  end

  def handle_cast({:escape_time, {number, data}}, options) do
    {:ok, _pid} = Task.start(fn ->
      send_chunk({number, Mandelbrot.pixels(data)})
    end)
    {:noreply, options}
  end

  def send_chunk(chunk) do
    ColorizerWorker.color(ColorizerWorker, chunk)
  end
end
