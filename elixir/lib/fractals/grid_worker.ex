defmodule Fractals.GridWorker do
  use GenServer

  alias Fractals.OutputWorker

  # Client

  def start_link([options]) do
    GenServer.start_link(__MODULE__, options, name: __MODULE__)
  end

  def work(pid) do
    GenServer.cast(pid, :work)
  end

  # Server

  def init(options) do
    {:ok, options}
  end

  def handle_cast(:work, options) do
    Task.start(fn -> grid_chunk_and_send(options) end)
    {:noreply, options}
  end

  def grid_chunk_and_send(options) do
    for row <- 1..options.size.height, col <- 1..options.size.width do
      # NEXT: build an actual grid point
      red  = div(rem(row, 1024), 4)
      blue = div(rem(col, 1024), 4)
      PPM.ppm(red, 0, blue)
    end
    |> Stream.chunk(options.chunk_size, options.chunk_size, [])
    |> Stream.zip(1..options.chunk_count)
    |> Stream.each(fn {chunk, chunk_number} ->
      OutputWorker.write(Fractals.OutputWorker, {chunk_number, chunk})
    end)
    |> Stream.run
  end
end
