defmodule Fractals.OutputWorker do
  use GenServer

  # Client API

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def write(pid, chunk) do
    GenServer.cast(pid, {:write, chunk})
  end

  # Server API

  defmodule State do
    defstruct next_number: 0, cache: %{}
  end

  def init(:ok) do
    {:ok, nil}
  end

  def handle_cast({:write, chunk}, nil) do
    state =
      %State{next_number: 0, cache: build_initial_cache(chunk.params)}
      |> process(chunk)
    {:noreply, state}
  end
  def handle_cast({:write, chunk}, state) do
    state = process(state, chunk)
    {:noreply, state}
  end

  # helpers

  defp process(%State{next_number: next_number, cache: cache}, chunk) do
    cache
    |> update_cache(chunk)
    |> output_cache(next_number, chunk.params)
  end

  defp update_cache(cache, %Chunk{number: number, data: data}) do
    Map.put(cache, number, data)
  end

  defp output_cache(cache, next_number, params) do
    case Map.get(cache, next_number) do
      nil ->
        %State{next_number: next_number, cache: cache}
      :done ->
        done(params)
        nil
      data ->
        write_chunk(next_number, data, params)
        cache = Map.delete(cache, next_number)
        output_cache(cache, next_number + 1, params)
    end
  end

  defp build_initial_cache(params) do
    %{0 => header(params), params.chunk_count + 1 => :done}
  end

  defp done(params) do
    notify_source_pid(params, {:done, self()})
  end

  defp header(params) do
    PPM.p3_header(params.size.width, params.size.height)
  end

  defp write_chunk(chunk_number, data, params) do
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
