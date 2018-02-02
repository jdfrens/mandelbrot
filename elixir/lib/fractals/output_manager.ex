defmodule Fractals.OutputManager do
  use GenStage

  alias Fractals.OutputWorker
  alias Fractals.OutputWorkerSupervisor

  # Client API

  def start_link(_) do
    GenStage.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  # Server API

  def init(process_lookup) do
    {:consumer, process_lookup, subscribe_to: [{Fractals.ColorizerWorker, max_demand: 10}]}
  end

  def handle_events(events, _from, process_lookup) do
    new_lookup =
      Enum.reduce(events, process_lookup, fn chunk, lookup ->
        {next_lookup, pid} = get_pid(lookup, chunk.params.id)
        OutputWorker.write(pid, chunk)
        next_lookup
      end)

    {:noreply, [], new_lookup}
  end

  def get_pid(lookup, id) do
    case Map.get(lookup, id) do
      nil ->
        name = {:global, {:output_worker, id}}
        {:ok, pid} = OutputWorkerSupervisor.new_worker(name: name)
        get_pid(Map.put(lookup, id, pid), id)

      pid ->
        {lookup, pid}
    end
  end
end
