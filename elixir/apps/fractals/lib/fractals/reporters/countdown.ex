defmodule Fractals.Reporters.Countdown do
  @moduledoc """
  GenServer to keep track of the fractals that have been queued up by a master process (like a command line or an
  integration test).

  When all of the files are processed, we notify the master process that it can stop.
  """

  use GenServer

  alias Fractals.Params

  @type state :: %{params_list: [Params.t()], for: pid()}

  # client

  @spec start_link(state) :: GenServer.on_start()
  def start_link(%{params_list: _params_list, for: _pid} = opts) do
    GenServer.start_link(__MODULE__, opts)
  end

  # server

  @ignored_tags [:starting, :writing]

  @impl GenServer
  def init(state) do
    {:ok, Enum.into(state, %{})}
  end

  @impl GenServer
  def handle_cast({:done, params, _opts}, state) do
    state
    |> fractal_done(params)
    |> response
  end

  @impl GenServer
  def handle_cast({:skipping, params, _opts}, state) do
    state
    |> fractal_done(params)
    |> response
  end

  @impl GenServer
  def handle_cast({tag, _params, _opts}, state) when tag in @ignored_tags do
    response(state)
  end

  @impl GenServer
  def terminate(reason, %{for: pid}) do
    send(pid, {:filenames_empty, reason})
  end

  @spec fractal_done(state(), Params.t()) :: state()
  def fractal_done(state, params) do
    state
    |> Map.get_and_update(:params_list, &{&1, List.delete(&1, params)})
    |> elem(1)
  end

  @spec response(state()) :: {:stop, String.t(), state()} | {:noreply, state()}
  def response(%{params_list: []} = state) do
    {:stop, :normal, state}
  end

  def response(state) do
    {:noreply, state}
  end
end
