defmodule Fractals.ConversionWorker do
  use GenServer

  def convert(params) do
    convert(__MODULE__, params)
  end

  # Client

  def start_link(options \\ []) do
    convert = Keyword.get(options, :convert, &Fractals.ImageMagick.convert/2)
    name    = Keyword.get(options, :name, __MODULE__)
    GenServer.start_link(__MODULE__, convert, name: name)
  end

  def convert(pid, params) do
    GenServer.cast(pid, {:convert, params})
  end

  # Server

  def init(convert) do
    {:ok, convert}
  end

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

  defp done(params) do
    notify_source_pid(params, {:done, self(), params})
  end

  defp notify_source_pid(params, message) do
    send params.source_pid, message
  end
end
