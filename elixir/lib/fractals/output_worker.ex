defmodule Fractals.OutputWorker do
  use GenServer

  # Client API

  # :next_stage is a callback function which is called when an image
  # is all written.  By default, this will close the output file and invoke
  # the conversion worker (from PPM to PNG).
  def start_link(options \\ []) do
    next_stage =
      Keyword.get(options, :next_stage, fn params ->
        Fractals.Params.close(params)
        Fractals.ConversionWorker.convert(params)
      end)

    name = Keyword.get(options, :name, __MODULE__)
    GenServer.start_link(__MODULE__, next_stage, name: name)
  end

  def write(pid, chunk) do
    GenServer.cast(pid, {:write, chunk})
  end

  # Server API

  def init(:ok) do
    {:ok, :state}
  end

  alias Fractals.Output.OutputState

  def init(next_stage) do
    {:ok, {nil, next_stage}}
  end

  def handle_cast({:write, chunk}, {nil, next_stage}) do
    state =
      %OutputState{next_number: 0, cache: build_initial_cache(chunk.params)}
      |> process(chunk, next_stage)

    {:noreply, {state, next_stage}}
  end

  def handle_cast({:write, chunk}, {state, next_stage}) do
    state = process(state, chunk, next_stage)
    {:noreply, {state, next_stage}}
  end

  # helpers

  defp process(%OutputState{next_number: next_number, cache: cache}, chunk, next_stage) do
    cache
    |> update_cache(chunk)
    |> output_cache(next_number, chunk.params, next_stage)
  end

  defp update_cache(cache, %Chunk{number: number, data: data}) do
    Map.put(cache, number, data)
  end

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

  defp build_initial_cache(params) do
    %{0 => header(params), (params.chunk_count + 1) => :done}
  end

  defp header(params) do
    PPM.p3_header(params.size.width, params.size.height)
  end

  defp write_chunk(chunk_number, data, params) do
    Progress.incr(:write_chunk)
    notify_source_pid(params, {:writing, chunk_number, params})
    lines_to_file(data, params)
  end

  defp lines_to_file(lines, params) do
    IO.write(params.output_pid, add_newlines(lines))
  end

  defp notify_source_pid(params, message) do
    send(params.source_pid, message)
  end

  defp add_newlines(lines) do
    Enum.map(lines, &[&1, "\n"])
  end
end
