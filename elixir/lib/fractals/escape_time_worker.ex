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
    pixels = Mandelbrot.pixels(data)
    ColorizerWorker.color(ColorizerWorker, {number, pixels})
    {:noreply, options}
  end
end
