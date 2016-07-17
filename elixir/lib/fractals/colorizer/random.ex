defmodule Fractals.Colorizer.Random do
  use GenServer

  ## Client

  def start_link(params) do
    GenServer.start_link(__MODULE__, params, name: __MODULE__)
  end

  def at(pid, iterations) do
    GenServer.call(pid, {:at, iterations})
  end

  ## Server

  def init(params) do
    {:ok, make_colors(params)}
  end

  def handle_call({:at, iterations}, _, colors) do
    {:reply, Enum.at(colors, iterations), colors}
  end

  ## Helpers

  defp make_colors(params) do
    Enum.map(range(params), &random_color(&1, params)) ++ [PPM.black]
  end

  defp range(params), do: 0..(params.max_iterations-1)

  defp random_color(_, params) do
    PPM.ppm(random_hue(params), random_hue(params), random_hue(params))
  end

  defp random_hue(params) do
    :rand.uniform(params.max_intensity+1) - 1
  end
end
