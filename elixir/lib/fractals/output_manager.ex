defmodule Fractals.OutputManager do
  use GenServer

  alias Fractals.OutputWorker
  alias Fractals.OutputWorkerSupervisor

  # Client API

  def start_link do
    GenServer.start_link(__MODULE__, %{}, name: __MODULE__)
  end

  def write(pid, chunk) do
    GenServer.cast(pid, {:write, chunk})
  end

  # Server API

  def handle_cast({:write, chunk}, process_lookup) do
    {new_lookup, pid} = get_pid(process_lookup, chunk.params.id)
    OutputWorker.write(pid, chunk)
    {:noreply, new_lookup}
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
