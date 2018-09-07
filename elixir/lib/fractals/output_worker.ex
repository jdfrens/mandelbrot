defmodule Fractals.OutputWorker do
  @moduledoc """
  Worker to close out a file when it is completely written.

  A callback function is also called for further processing once the file is closed.
  """

  use GenServer

  alias Fractals.Params
  alias Fractals.Output.OutputState

  # Client API

  # :next_stage is a callback function which is called when an image
  # is all written.  By default, this will close the output file and invoke
  # the conversion worker (from PPM to PNG).
  @spec start_link({(Params.t() -> any), atom}) :: GenServer.on_start()
  def start_link({next_stage, name}) do
    GenServer.start_link(__MODULE__, next_stage, name: name)
  end

  @spec write(pid | atom, Chunk.t()) :: :ok
  def write(pid, chunk) do
    GenServer.cast(pid, {:write, chunk})
  end

  # Server API

  @impl GenServer
  def init(next_stage) do
    {:ok, {nil, next_stage}}
  end

  @impl GenServer
  def handle_cast({:write, chunk}, {nil, next_stage}) do
    state =
      %OutputState{next_number: 0, cache: build_initial_cache(chunk.params)}
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
    %{0 => header(params), (params.chunk_count + 1) => :done}
  end

  @spec header(Params.t()) :: [String.t()]
  defp header(params) do
    PPM.p3_header(params.size.width, params.size.height)
  end

  @spec write_chunk(non_neg_integer, [String.t()], Params.t()) :: :ok
  defp write_chunk(chunk_number, data, params) do
    notify_source_pid(params, {:writing, chunk_number, params})
    lines_to_file(data, params)
  end

  @spec lines_to_file([String.t()], Params.t()) :: :ok
  defp lines_to_file(lines, params) do
    IO.write(params.output_pid, add_newlines(lines))
  end

  @spec notify_source_pid(Params.t(), {:writing, non_neg_integer, Params.t()}) :: any
  defp notify_source_pid(params, message) do
    send(params.source_pid, message)
  end

  @spec add_newlines([String.t()]) :: [[String.t()]]
  defp add_newlines(lines) do
    Enum.map(lines, &[&1, "\n"])
  end
end
