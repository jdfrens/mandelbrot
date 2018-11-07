defmodule Fractals.ConversionWorker do
  @moduledoc """
  Process that converts image files to other formats (e.g., PPM to PNG).
  """

  use GenServer

  alias Fractals.{ImageMagick, Params, Reporters.Broadcaster}

  # Client

  @spec start_link(keyword) :: GenServer.on_start()
  def start_link(options \\ []) do
    convert = Keyword.get(options, :convert, &ImageMagick.convert/2)
    broadcast = Keyword.get(options, :broadcast, &Broadcaster.report/3)
    name = Keyword.get(options, :name, __MODULE__)

    state = %{
      convert: convert,
      broadcast: broadcast
    }

    GenServer.start_link(__MODULE__, state, name: name)
  end

  @spec convert(pid | atom, Params.t()) :: :ok
  def convert(pid \\ __MODULE__, params) do
    GenServer.cast(pid, {:convert, params})
  end

  # Server

  @impl GenServer
  def init(state) do
    {:ok, state}
  end

  @impl GenServer
  def handle_cast({:convert, params}, %{convert: convert, broadcast: broadcast} = state) do
    params.output_filename
    |> Path.extname()
    |> convert_to(params, convert)

    done(broadcast, params)
    {:noreply, state}
  end

  @spec convert_to(String.t(), Params.t(), (String.t(), String.t() -> any)) :: Params.t()
  defp convert_to(".ppm", params, _convert) do
    # OutputWorker already wrote a PPM file
    params
  end

  defp convert_to(".png", params, convert) do
    root_filename =
      params.output_filename
      |> Path.rootname(".png")
      |> Path.rootname(".ppm")

    ppm_filename = root_filename <> ".ppm"
    convert.(ppm_filename, params.output_filename)

    params
  end

  @spec done((atom, Params.t(), keyword -> any), Params.t()) :: any
  defp done(broadcast, params) do
    broadcast.(:done, params, from: self())
  end
end
