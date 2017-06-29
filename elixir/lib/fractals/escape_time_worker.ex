defmodule Fractals.EscapeTimeWorker do
  use GenServer

  alias Fractals.ColorizerWorker
  alias Fractals.EscapeTime.{BurningShip, Julia, Mandelbrot}

  # Client

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def escape_time(pid, chunk) do
    GenServer.cast(pid, {:escape_time, chunk})
  end

  # Server

  def handle_cast({:escape_time, %Chunk{params: params, data: data} = chunk}, :ok) do
    new_data = pixels(params.fractal, data, params)
    Progress.incr(:escape_chunk)
    send_chunk(%{chunk | data: new_data})
    {:noreply, :ok}
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
