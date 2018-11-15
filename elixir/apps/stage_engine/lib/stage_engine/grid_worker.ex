defmodule StageEngine.GridWorker do
  @moduledoc """
  Producer process in the gen_stage workflow.  It chunks the pixel grid and feeds chunks into the workflow.
  """

  use GenStage

  alias Fractals.{Grid, Params}

  # Client

  @spec start_link(any) :: GenServer.on_start()
  def start_link(_) do
    GenStage.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @spec work(pid | atom, Params.t()) :: :ok
  def work(pid, params) do
    GenStage.call(pid, {:work, params})
  end

  # Callbacks

  @impl GenStage
  def init(:ok) do
    {:producer, {:queue.new(), 0}}
  end

  # Server

  @impl GenStage
  def handle_call({:work, params}, _from, {queue, pending_demand}) do
    new_queue =
      Enum.reduce(Grid.chunked_grid(params), queue, fn chunk, q -> :queue.in(chunk, q) end)

    {demanded, cached} =
      :queue.split(
        min(pending_demand, :queue.len(new_queue)),
        new_queue
      )

    {:reply, :ok, :queue.to_list(demanded), {cached, pending_demand - :queue.len(demanded)}}
  end

  @impl GenStage
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
