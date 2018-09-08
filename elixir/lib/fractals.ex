defmodule Fractals do
  @moduledoc """
  The application.
  """

  use Application

  alias Fractals.{
    Colorizer.Random,
    ColorizerWorker,
    ConversionWorker,
    EscapeTimeWorker,
    GridWorker,
    OutputManager,
    OutputWorkerSupervisor,
    Params
  }

  @unimplemented Application.get_env(:fractals, :unimplemented)

  @impl Application
  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    staged = [
      GridWorker,
      EscapeTimeWorker,
      ColorizerWorker,
      OutputManager
    ]

    unstaged = [
      Random,
      OutputWorkerSupervisor,
      ConversionWorker
    ]

    Supervisor.start_link(staged ++ unstaged, strategy: :one_for_one)
  end

  @spec fractalize(Fractals.Params.t()) :: :ok
  def fractalize(params) do
    if unimplemented?(params.fractal) do
      send(
        params.source_pid,
        {:skipping, params, reason: "fractal not implemented", from: self()}
      )
    else
      send(params.source_pid, {:starting, params, from: self()})
      GridWorker.work(Fractals.GridWorker, params)
    end

    :ok
  end

  @spec unimplemented?(Params.fractal_type()) :: boolean
  defp unimplemented?(fractal) do
    Enum.member?(@unimplemented, fractal)
  end
end
