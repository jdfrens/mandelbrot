defmodule Fractals.GridWorker do
  use GenStage

  alias Fractals.Grid

  # Client

  def start_link do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def work(pid, params) do
    GenStage.call(pid, {:work, params})
  end

  # Callbacks

  def init(:ok) do
    {:producer, {:queue.new, 0}}
  end

  # Server

  # TODO: call, not cast?
  def handle_call({:work, params}, _from, {queue, pending_demand}) do
    # TODO: measure progress in Grid module itself?
    # Progress.incr(:generate_chunk)
    new_queue = Enum.reduce(
      Grid.chunked_grid(params),
      queue,
      fn(chunk, q) -> :queue.in(chunk, q) end
    )
    {demanded, cached} =
      :queue.split(
        min(pending_demand, :queue.len(new_queue)),
        new_queue
      )
    {:reply, :ok, :queue.to_list(demanded), {cached, pending_demand - :queue.len(demanded)}}
  end

  def handle_demand(demand, {queue, pending_demand}) when demand > 0 do
    total_demand = demand + pending_demand
    {demanded, cached} =
      :queue.split(
        min(total_demand, :queue.len(queue)),
        queue
      )
    {:noreply, :queue.to_list(demanded), {cached, total_demand - :queue.len(demanded)}}
  end
end
