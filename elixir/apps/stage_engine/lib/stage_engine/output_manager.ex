defmodule StageEngine.OutputManager do
  @moduledoc """
  Manages supervisors for outputting files.
  """

  use GenStage

  alias Fractals.OutputWorker

  # Client API

  @spec start_link(any) :: GenServer.on_start()
  def start_link(_) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  # Server API

  @impl GenStage
  def init(:ok) do
    {:consumer, :ok, subscribe_to: [{StageEngine.ColorizerWorker, max_demand: 10}]}
  end

  @impl GenStage
  def handle_events(events, _from, :ok) do
    Enum.each(events, &OutputWorker.write/1)

    {:noreply, [], :ok}
  end
end
