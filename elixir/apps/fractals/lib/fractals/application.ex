defmodule Fractals.Application do
  @moduledoc false

  use Application

  def start(_type, _args) do
    children = [
      # color
      Fractals.Colorizer.Random,
      # image output
      {DynamicSupervisor, strategy: :one_for_one, name: Fractals.OutputWorkerSupervisor},
      Fractals.ConversionWorker,
      {Registry, keys: :unique, name: Fractals.OutputWorkerRegistry},
      # reporters
      Fractals.Reporters.Supervisor,
      Fractals.Reporters.Broadcaster
    ]

    opts = [strategy: :one_for_one, name: CLI.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
