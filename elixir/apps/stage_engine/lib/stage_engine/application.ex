defmodule StageEngine.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      StageEngine.GridWorker,
      StageEngine.EscapeTimeWorker,
      StageEngine.ColorizerWorker,
      StageEngine.OutputManager
    ]

    opts = [strategy: :one_for_one, name: StageEngine.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
