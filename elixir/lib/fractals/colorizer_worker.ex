defmodule Fractals.ColorizerWorker do
  @moduledoc """
  Worker to compute colors on a chunk of pixels
  """
  
  use GenServer

  alias Fractals.OutputManager
  alias Fractals.Colorizer

  # Client

  def start_link do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def color(pid, chunk) do
    GenServer.cast(pid, {:color, chunk})
  end

  # Server

  def init(:ok) do
    {:ok, :state}
  end

  def handle_cast({:color, %Chunk{params: params, data: data} = chunk}, state) do
    write(%{chunk | data: colorize(data, params)})
    {:noreply, state}
  end

  @spec colorize({atom, {non_neg_integer, list}}, Params) :: list(String.t)
  def colorize(data, params) do
    Enum.map(data, &Colorizer.color_point(&1, params))
  end

  def write(chunk) do
    Progress.incr(:colorize_chunk)
    OutputManager.write(OutputManager, chunk)
  end
end
