defmodule Fractals.Generator.PooledProcesses do
  @moduledoc """
  Generates limited number of processes to compute pixels.  Working
  processes ask for pixel info and send back results.
  """

  import Fractals.Generator

  alias Fractals.Grid

  def generate(options) do
    children = spawn_children(options)
    grid_points = options |> Grid.generate(&build_complex/2)
    dispatch_work(grid_points, options)
    image = receive_image(grid_points, options)
    stop_children(children)
    image
  end

  def dispatch_work(grid_points, options) do
    grid_points
    |> Enum.chunk(options.chunk_size, options.chunk_size, [])
    |> Enum.with_index
    |> Enum.each(fn {chunk, i} ->
      receive do
        {:ready, pid} ->
          send pid, {self, chunk, i}
      end
    end)
  end

  def receive_image(grid_points, options) do
    grid_points
    |> Enum.chunk(options.chunk_size, options.chunk_size, [])
    |> Enum.with_index
    # TODO: this might be a bit inefficient
    |> Enum.map(fn {_chunk, i} ->
      receive do
        {:ok, ^i, pixels} ->
          pixels
      end
    end)
    |> Enum.concat
  end

  def stop_children(children) do
    Enum.each(children, fn pid ->
      receive do
        {:ready, ^pid} ->
          send pid, {:done}
      end
    end)
  end

  def spawn_children(options) do
    1..4
    |> Enum.map(fn _ ->
      pid = spawn(__MODULE__, :pixel_process, [self, options])
      pid
    end)
  end

  def pixel_process(generator_pid, options) do
    send generator_pid, {:ready, self}
    receive do
      {^generator_pid, chunk, i} ->
        ps = pixels(chunk, options)
        send generator_pid, {:ok, i, ps}
        pixel_process(generator_pid, options)
      {:done} ->
    end
  end
end
