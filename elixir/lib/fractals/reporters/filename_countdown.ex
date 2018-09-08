defmodule Fractals.Reporters.FilenameCountdown do
  @moduledoc """
  GenServer to keep track of the filenames that have been queued up.

  When all of the files are processed, we notify a process (probably a CLI process) that it can stop.
  """

  use GenServer

  # client

  @spec start_link(keyword | map) :: GenServer.on_start()
  def start_link(opts) do
    GenServer.start_link(__MODULE__, opts, name: __MODULE__)
  end

  # server

  @ignored_tags [:starting, :writing]

  def init(state) do
    {:ok, Enum.into(state, %{})}
  end

  def handle_cast({:done, params, _opts}, state) do
    params.params_filename
    |> file_done(state)
    |> response
  end

  def handle_cast({:skipping, params, _opts}, state) do
    params.params_filename
    |> file_done(state)
    |> response
  end

  def handle_cast({tag, _params, _opts}, state) when tag in @ignored_tags do
    response(state)
  end

  def terminate(reason, %{for: pid}) do
    send(pid, {:filenames_empty, reason})
  end

  def file_done(filename, %{filenames: filenames} = state) do
    Map.put(state, :filenames, List.delete(filenames, filename))
  end

  def response(%{filenames: []} = state) do
    {:stop, "all fractals processed", state}
  end

  def response(state) do
    {:noreply, state}
  end
end
