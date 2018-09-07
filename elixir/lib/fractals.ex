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
    OutputWorkerSupervisor
  }

  @unimplemented Application.get_env(:fractals, :unimplemented)

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    staged = [
      # TODO: start process
      GridWorker,
      EscapeTimeWorker,
      ColorizerWorker,
      OutputManager
      # TODO: end process
    ]

    unstaged = [
      Random,
      OutputWorkerSupervisor,
      ConversionWorker
    ]

    Supervisor.start_link(staged ++ unstaged, strategy: :one_for_one)
  end

  def fractalize(params) do
    send(params.source_pid, {:starting, self(), params})

    if unimplemented?(params.fractal) do
      # TODO: extract a common library for these notifications
      send(params.source_pid, {:skipping, self(), params, "not implemented"})
    else
      GridWorker.work(Fractals.GridWorker, params)
    end
  end

  defp unimplemented?(fractal) do
    Enum.member?(@unimplemented, fractal)
  end
end
