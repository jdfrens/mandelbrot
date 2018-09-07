defmodule Fractals.EscapeTimeWorker do
  @moduledoc """
  Process that runs the right escape-time algorithm for the specified fractal.
  """

  use GenStage

  alias Fractals.EscapeTime.{BurningShip, Julia, Mandelbrot}
  alias Fractals.{GridWorker, Params}

  # Client

  @spec start_link(keyword) :: GenServer.on_start()
  def start_link(_) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  # Server

  @impl GenStage
  def init(:ok) do
    {:producer_consumer, :ok, subscribe_to: [{GridWorker, max_demand: 10}]}
  end

  @impl GenStage
  def handle_events(events, _from, :ok) do
    escaped =
      Enum.map(events, fn %Chunk{params: params} = chunk ->
        %{chunk | data: pixels(params.fractal, chunk.data, params)}
      end)

    {:noreply, escaped, :ok}
  end

  @spec pixels(Params.fractal_type(), list, Params.t()) :: any
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
