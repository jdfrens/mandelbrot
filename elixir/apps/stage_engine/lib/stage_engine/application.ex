defmodule StageEngine.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      Fractals.GridWorker,
      Fractals.EscapeTimeWorker,
      Fractals.ColorizerWorker,
      Fractals.OutputManager
    ]

    opts = [strategy: :one_for_one, name: StageEngine.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
