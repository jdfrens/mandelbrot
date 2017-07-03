defmodule Fractals.GridWorker do
  use GenStage

  alias Fractals.Grid

  # Client

  def start_link do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def work(pid, params) do
    GenStage.cast(pid, {:work, params})
  end

  # Callbacks

  def init(:ok) do
    {:producer, :ok}
  end

  # Server

  # TODO: call, not cast?
  def handle_cast({:work, params}, :ok) do
    # TODO: measure progress in Grid module itself?
    # Progress.incr(:generate_chunk)
    {:noreply, Grid.chunked_grid(params), :ok}
  end

  # TODO: do nothing?
  def handle_demand(_demand, :ok) do
    {:noreply, [], :ok}
  end
end
