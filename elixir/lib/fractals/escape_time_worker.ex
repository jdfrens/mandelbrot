defmodule Fractals.EscapeTimeWorker do
  use GenServer

  alias Fractals.ColorizerWorker
  alias Fractals.EscapeTime.{BurningShip, Julia, Mandelbrot}

  # Client

  def start_link(params) do
    GenServer.start_link(__MODULE__, params, name: __MODULE__)
  end

  def escape_time(pid, chunk) do
    GenServer.cast(pid, {:escape_time, chunk})
  end

  # Server

  def handle_cast({:escape_time, {number, data}}, params) do
    {:ok, _pid} = Task.start(fn ->
      send_chunk({number, pixels(params.fractal, data, params)})
    end)
    {:noreply, params}
  end

  def pixels(:mandelbrot, data, params) do
    Mandelbrot.pixels(data, params)
  end
  def pixels(:julia, data, params) do
    Julia.pixels(data, params)
  end
  def pixels(:burningship, data, params) do
    BurningShip.pixels(data, params)
  end

  def send_chunk(chunk) do
    ColorizerWorker.color(ColorizerWorker, chunk)
  end
end
