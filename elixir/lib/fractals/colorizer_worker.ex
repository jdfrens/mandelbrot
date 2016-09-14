defmodule Fractals.ColorizerWorker do
  use GenServer

  alias Fractals.OutputWorker
  alias Fractals.Colorizer

  # Client

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def color(pid, chunk) do
    GenServer.cast(pid, {:color, chunk})
  end

  # Server

  def handle_cast({:color, %Chunk{params: params, data: data} = chunk}, state) do
    {:ok, _pid} = Task.start(fn -> write(%{chunk | data: colorize(data, params)}) end)
    {:noreply, state}
  end

  @spec colorize({atom, {non_neg_integer, list}}, Params) :: list(String.t)
  def colorize(data, params) do
    Enum.map(data, &Colorizer.color_point(&1, params))
  end

  def write(chunk) do
    OutputWorker.write(Fractals.OutputWorker, chunk)
  end
end
