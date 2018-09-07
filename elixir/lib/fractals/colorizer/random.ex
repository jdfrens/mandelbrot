defmodule Fractals.Colorizer.Random do
  @moduledoc """
  GenServer that generates and maintains an array of colors.  The same list of colors is used for all fractals until
  this process is killed and a new list is generated.
  """

  use GenServer

  import Fractals.EscapeTime.Helpers

  alias Fractals.Params

  ## Client

  def start_link(_) do
    GenServer.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  @spec at(pid | atom, integer, Params.t()) :: PPM.color()
  def at(pid, iterations, params) do
    GenServer.call(pid, {:at, iterations, params})
  end

  ## Server

  @impl GenServer
  def init(:ok) do
    {:ok, make_colors()}
  end

  @impl GenServer
  def handle_call({:at, iterations, params}, _, colors) do
    color = colors |> pick_color(iterations, params) |> PPM.ppm()
    {:reply, color, colors}
  end

  ## Helpers

  @max_colors 2048

  @spec pick_color([[float]], integer, Params.t()) :: [integer]
  def pick_color(colors, iterations, params) do
    if inside?(iterations, params.max_iterations) do
      [0, 0, 0]
    else
      colors
      |> Enum.at(iterations)
      |> Enum.map(&round(&1 * params.max_intensity))
    end
  end

  @spec make_colors :: [[float]]
  defp make_colors do
    Stream.repeatedly(&random_color/0) |> Enum.take(@max_colors)
  end

  @spec random_color :: [float]
  defp random_color do
    [:rand.uniform(), :rand.uniform(), :rand.uniform()]
  end
end
