defmodule Fractals.Output do
  use GenServer

  def start_link(%{io: _io}=default) do
    GenServer.start_link(__MODULE__, default)
  end

  def out(pid, string) do
    GenServer.cast(pid, {:out, string})
  end

  def handle_cast({:out, string}, %{io: io}=state) do
    IO.write(io, string)
    {:noreply, state}
  end
end
