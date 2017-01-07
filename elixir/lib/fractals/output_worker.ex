defmodule Fractals.OutputWorker do
  use GenServer

  # Client API

  def start_link(next_stage \\ &Fractals.ConversionWorker.convert/1) do
    GenServer.start_link(__MODULE__, next_stage, name: __MODULE__)
  end

  def write(pid, chunk) do
    GenServer.cast(pid, {:write, chunk})
  end

  # Server API

  defmodule State do
    defstruct next_number: 0, cache: %{}
  end

  def init(next_stage) do
    {:ok, {nil, next_stage}}
  end

  def handle_cast({:write, chunk}, {nil, next_stage}) do
    state =
      %State{next_number: 0, cache: build_initial_cache(chunk.params)}
      |> process(chunk, next_stage)
    {:noreply, {state, next_stage}}
  end
  def handle_cast({:write, chunk}, {state, next_stage}) do
    state = process(state, chunk, next_stage)
    {:noreply, {state, next_stage}}
  end

  # helpers

  defp process(%State{next_number: next_number, cache: cache}, chunk, next_stage) do
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
        %State{next_number: next_number, cache: cache}
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
    %{0 => header(params), params.chunk_count + 1 => :done}
  end

  defp header(params) do
    PPM.p3_header(params.size.width, params.size.height)
  end

  defp write_chunk(chunk_number, data, params) do
    Progress.incr(:write_chunk)
    notify_source_pid(params, {:writing, chunk_number})
    lines_to_file(data, params)
  end

  defp lines_to_file(lines, params) do
    Enum.each(lines, &(IO.puts(params.output_pid, &1)))
  end

  defp notify_source_pid(params, message) do
    send params.source_pid, message
  end
end
