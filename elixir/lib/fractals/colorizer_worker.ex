defmodule Fractals.ColorizerWorker do
  use GenServer

  alias Fractals.OutputWorker
  alias Fractals.Colorizer

  # Client

  def start_link([options]) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  def color(pid, chunk) do
    GenServer.cast(pid, {:color, chunk})
  end

  # Server

  def init(options) do
    {:ok, options}
  end

  def handle_cast({:color, {number, data}}, options) do
    Task.start(fn -> write({number, colorize(options, data)}) end)
    {:noreply, options}
  end

  def colorize(options, data) do
    Enum.map(data, &Colorizer.color_point(&1, options))
  end

  def write(chunk) do
    OutputWorker.write(Fractals.OutputWorker, chunk)
  end
end
