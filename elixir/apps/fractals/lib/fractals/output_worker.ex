defmodule Fractals.OutputWorker do
  @moduledoc """
  Worker to close out a file when it is completely written.

  A callback function is also called for further processing once the file is closed.
  """

  use GenServer

  alias Fractals.{ConversionWorker, Params}
  alias Fractals.Output.{OutputState, PPMFile}
  alias Fractals.Reporters.Broadcaster

  @type via_tuple :: {:via, Registry, {Fractals.OutputWorkerRegistry, Params.fractal_id()}}

  # Client API

  @doc """
  Writes a chunk of data to an output worker.

  Use `write/1`.  It will create an output worker for the fractal if one has not already been started.

  Use `write/2` if you want to micromanage which output worker processes the chunk.  Used to test this worker.
  """
  @spec write(GenServer.name(), Chunk.t()) :: :ok
  def write(pid \\ nil, chunk)

  def write(nil, chunk) do
    write(via_tuple(chunk.params.id), chunk)
  end

  def write(pid, chunk) do
    maybe_new(name: pid)
    GenServer.cast(pid, {:write, chunk})
  end

  @doc """
  Starts an output worker.

  Don't use this.  It should be used as part of a `DynamicSupervisor` which is handled by `write/1`.  This function is
  useful for testing.
  """
  @spec start_link({(Params.t() -> any), atom}) :: GenServer.on_start()
  def start_link({next_stage, name}) do
    GenServer.start_link(__MODULE__, next_stage, name: name)
  end

  @spec via_tuple(Params.fractal_id()) :: via_tuple()
  defp(via_tuple(fractal_id)) do
    {:via, Registry, {Fractals.OutputWorkerRegistry, fractal_id}}
  end

  # Creates a new worker if needed.
  @spec maybe_new(keyword) :: Supervisor.on_start_child()
  defp maybe_new(options) do
    name = Keyword.get(options, :name)
    next_stage = Keyword.get(options, :next_stage, &default_next_stage/1)

    case DynamicSupervisor.start_child(
           Fractals.OutputWorkerSupervisor,
           {__MODULE__, {next_stage, name}}
         ) do
      {:error, {:already_started, pid}} ->
        {:ok, pid}

      response ->
        response
    end
  end

  # Closes the params (i.e., closes the file).  Triggers the conversion worker.
  @spec default_next_stage(Params.t()) :: any()
  defp default_next_stage(params) do
    Params.close(params)
    ConversionWorker.convert(params)
  end

  # Server API

  @impl GenServer
  def init(next_stage) do
    {:ok, {nil, next_stage}}
  end

  @impl GenServer
  def handle_cast({:write, chunk}, {nil, next_stage}) do
    PPMFile.start_file(chunk.params)

    state =
      %OutputState{next_number: 1, cache: build_initial_cache(chunk.params)}
      |> process(chunk, next_stage)

    {:noreply, {state, next_stage}}
  end

  @impl GenServer
  def handle_cast({:write, chunk}, {state, next_stage}) do
    state = process(state, chunk, next_stage)
    {:noreply, {state, next_stage}}
  end

  # helpers

  @spec process(OutputState.t(), Chunk.t(), (Params.t() -> any)) :: OutputState.t() | nil
  defp process(%OutputState{next_number: next_number, cache: cache}, chunk, next_stage) do
    cache
    |> update_cache(chunk)
    |> output_cache(next_number, chunk.params, next_stage)
  end

  @spec update_cache(map, Chunk.t()) :: map
  defp update_cache(cache, %Chunk{number: number, data: data}) do
    Map.put(cache, number, data)
  end

  @spec output_cache(map, non_neg_integer, Params.t(), (Params.t() -> any)) ::
          OutputState.t() | nil
  defp output_cache(cache, next_number, params, next_stage) do
    case Map.get(cache, next_number) do
      nil ->
        %OutputState{next_number: next_number, cache: cache}

      :done ->
        next_stage.(params)
        nil

      data ->
        write_chunk(next_number, data, params)

        cache
        |> Map.delete(next_number)
        |> output_cache(next_number + 1, params, next_stage)
    end
  end

  @spec build_initial_cache(Params.t()) :: map
  defp build_initial_cache(params) do
    %{(params.chunk_count + 1) => :done}
  end

  @spec write_chunk(non_neg_integer, [String.t()], Params.t()) :: :ok
  defp write_chunk(chunk_number, data, params) do
    Broadcaster.report(:writing, params, chunk_number: chunk_number)
    PPMFile.write_pixels(params, data)
  end
end
