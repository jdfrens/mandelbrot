defmodule Fractals.OutputManager do
  use GenStage

  alias Fractals.OutputWorker
  alias Fractals.OutputWorkerSupervisor

  # Client API

  def start_link do
    GenStage.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  # Server API

  def init(process_lookup) do
    {:consumer, process_lookup, subscribe_to: [Fractals.ColorizerWorker]}
  end

  def handle_events(events, _from, process_lookup) do
    new_lookup = Enum.reduce(events, process_lookup, fn(chunk, lookup) ->
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
        {:ok, pid} = Supervisor.start_child(OutputWorkerSupervisor, [[name: name]])
        get_pid(Map.put(lookup, id, pid), id)
      pid ->
        {lookup, pid}
    end
  end
end
