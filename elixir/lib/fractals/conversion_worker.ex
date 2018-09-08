defmodule Fractals.ConversionWorker do
  @moduledoc """
  Process that converts image files to other formats (e.g., PPM to PNG).
  """

  use GenServer

  alias Fractals.{ImageMagick, Params}

  @spec convert(Params.t()) :: any
  def convert(params) do
    convert(__MODULE__, params)
  end

  # Client

  @spec start_link(keyword) :: GenServer.on_start()
  def(start_link(options \\ [])) do
    convert = Keyword.get(options, :convert, &ImageMagick.convert/2)
    name = Keyword.get(options, :name, __MODULE__)
    GenServer.start_link(__MODULE__, convert, name: name)
  end

  @spec convert(pid | atom, Params.t()) :: :ok
  def convert(pid, params) do
    GenServer.cast(pid, {:convert, params})
  end

  # Server

  @impl GenServer
  def init(convert) do
    {:ok, convert}
  end

  @impl GenServer
  def handle_cast({:convert, params}, convert) do
    case Path.extname(params.output_filename) do
      ".ppm" ->
        # OutputWorker already wrote a PPM file
        done(params)
        {:noreply, convert}

      ".png" ->
        root_filename =
          params.output_filename
          |> Path.rootname(".png")
          |> Path.rootname(".ppm")

        ppm_filename = root_filename <> ".ppm"
        convert.(ppm_filename, params.output_filename)
        done(params)
        {:noreply, convert}
    end
  end

  @spec done(Params.t()) :: any
  defp done(params) do
    notify_source_pid(params, {:done, params, from: self()})
  end

  @spec notify_source_pid(Params.t(), {atom, pid, Params.t()}) :: any
  defp notify_source_pid(params, message) do
    send(params.source_pid, message)
  end
end
