defmodule Fractals.EscapeTimeWorker do
  use GenStage

  alias Fractals.EscapeTime.{BurningShip, Julia, Mandelbrot}

  # Client

  def start_link do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  # Server

  def init(:ok) do
    {:producer_consumer, :ok, subscribe_to: [{Fractals.GridWorker, max_demand: 10}]}
  end

  def handle_events(events, _from, :ok) do
    escaped =
      Enum.map(events, fn %Chunk{params: params} = chunk ->
        %{chunk | data: pixels(params.fractal, chunk.data, params)}
        # TODO: record this elsewhere?
        # Progress.incr(:escape_chunk)
      end)

    {:noreply, escaped, :ok}
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
end
