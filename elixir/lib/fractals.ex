defmodule Fractals do
  use Application

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      supervisor(Fractals.JobSupervisor, [])
    ]
    opts = [strategy: :one_for_one, name: Fractals.Supervisor]
    Supervisor.start_link(children, opts)
  end
end
