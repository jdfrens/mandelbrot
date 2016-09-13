defmodule Fractals.OutputWorker do
  use GenServer

  # Client API

  def start_link(params) do
    GenServer.start_link(__MODULE__, params, name: __MODULE__)
  end

  def write(pid, chunk) do
    GenServer.cast(pid, {:write, chunk})
  end

  # Server API

  def init(params) do
    {:ok, %{next_number: 0, cache: build_cache(params), params: params}}
  end

  def handle_cast({:write, chunk}, state) do
    {next_number, cache} =
      state.cache
      |> update_cache(chunk)
      |> output_cache(state.next_number, state.params)
    {:noreply, %{state | next_number: next_number, cache: cache}}
  end

  # helpers

  defp update_cache(cache, {number, data}) do
    Map.put(cache, number, data)
  end

  defp output_cache(cache, next_number, params) do
    case Map.get(cache, next_number) do
      nil ->
        {next_number, cache}
      :done ->
        done(params)
      data ->
        write_chunk(next_number, data, params)
        # TODO: delete `next_number` from `cache`?
        output_cache(cache, next_number + 1, params)
    end
  end

  defp build_cache(params) do
    %{0 => header(params), params.chunk_count+1 => :done}
  end

  defp done(params) do
    notify_next_pid(params, {:done, self})
  end

  defp header(params) do
    PPM.p3_header(params.size.width, params.size.height)
  end

  defp write_chunk(chunk_number, data, params) do
    notify_next_pid(params, {:writing, chunk_number})
    lines_to_file(data, params)
  end

  defp lines_to_file(lines, params) do
    Enum.each(lines, &(IO.puts(params.output_pid, &1)))
  end

  defp notify_next_pid(params, message) do
    send params.next_pid, message
  end
end
