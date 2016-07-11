defmodule Fractals.Colorizer.Random do
  use GenServer

  import Fractals.Colorizer, only: :macros

  @maximum_iterations 256

  ## Client

  def start_link do
    GenServer.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def at(pid, iterations) do
    GenServer.call(pid, {:at, iterations})
  end

  ## Server

  def init(_) do
    {:ok, make_colors}
  end

  def handle_call({:at, iterations}, _, colors) when escaped?(iterations) do
    {:reply, PPM.black, colors}
  end
  def handle_call({:at, iterations}, _, colors) do
    {:reply, Enum.at(colors, iterations), colors}
  end

  defp make_colors do
    Enum.map(0..255, &random_color/1)
  end

  defp random_color(_) do
    PPM.ppm(random_hue, random_hue, random_hue)
  end

  defp random_hue do
    :random.uniform(255)
  end
end
