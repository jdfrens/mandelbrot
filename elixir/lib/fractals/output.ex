defmodule Fractals.Output do
  def start_link(%{io: io}) do
    {:ok, spawn_link(__MODULE__, :server, [io, 0])}
  end

  def write(pid, chunk) do
    send pid, {:write, chunk}
  end

  def server(io, chunk_number) do
    receive do
      {:write, {^chunk_number, _options, data}} ->
        Enum.each(data, &(IO.puts(io,&1)))
        server(io, chunk_number+1)
    end
  end
end
