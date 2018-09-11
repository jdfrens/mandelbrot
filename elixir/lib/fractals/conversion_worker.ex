defmodule Fractals.ConversionWorker do
  @moduledoc """
  Process that converts image files to other formats (e.g., PPM to PNG).
  """

  use GenServer

  alias Fractals.{ImageMagick, Params, Reporters.Broadcaster}

  # Client

  @spec start_link(keyword) :: GenServer.on_start()
  def(start_link(options \\ [])) do
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
    case Path.extname(params.output_filename) do
      ".ppm" ->
        # OutputWorker already wrote a PPM file
        :ok

      ".png" ->
        root_filename =
          params.output_filename
          |> Path.rootname(".png")
          |> Path.rootname(".ppm")

        ppm_filename = root_filename <> ".ppm"
        convert.(ppm_filename, params.output_filename)
    end

    done(broadcast, params)
    {:noreply, state}
  end

  @spec done((atom, Params.t(), keyword -> any), Params.t()) :: any
  defp done(broadcast, params) do
    broadcast.(:done, params, from: self())
  end
end
