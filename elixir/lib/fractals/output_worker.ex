defmodule Fractals.OutputWorker do
  def start_link(options) do
    state = Map.put(options, :chunk_number, 0)
    {:ok, spawn_link(__MODULE__, :server, [state])}
  end

  def write(pid, chunk) do
    send pid, {:write, chunk}
  end

  def server(%{next_pid: next_pid,
               chunk_count: chunk_count, chunk_number: chunk_count}) do
    send next_pid, {:done, self}
  end
  def server(%{io: io, chunk_number: chunk_number} = state) do
    receive do
      {:write, {^chunk_number, _options, data}} ->
        Enum.each(data, &(IO.puts(io,&1)))
        server(%{state | chunk_number: chunk_number+1})
    end
  end
end
