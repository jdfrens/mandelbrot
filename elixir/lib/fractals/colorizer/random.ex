defmodule Fractals.Colorizer.Random do
  use GenServer

  import Fractals.EscapeTime.Helpers

  ## Client

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def at(pid, iterations, params) do
    GenServer.call(pid, {:at, iterations, params})
  end

  ## Server

  def init(:ok) do
    {:ok, make_colors()}
  end

  def handle_call({:at, iterations, params}, _, colors) do
    color = colors |> pick_color(iterations, params) |> PPM.ppm()
    {:reply, color, colors}
  end

  ## Helpers

  @max_colors 2048

  def pick_color(colors, iterations, params) do
    if inside?(iterations, params.max_iterations) do
      [0, 0, 0]
    else
      colors
      |> Enum.at(iterations)
      |> Enum.map(&round(&1 * params.max_intensity))
    end
  end

  defp make_colors do
    Stream.repeatedly(&random_color/0) |> Enum.take(@max_colors)
  end

  defp random_color do
    [:rand.uniform(), :rand.uniform(), :rand.uniform()]
  end
end
