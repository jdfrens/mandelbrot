defmodule Fractals.Job do
  use Supervisor

  def start_link(params) do
    Supervisor.start_link(__MODULE__, params)
  end

  def init(params) do
    children = [
      supervisor(Fractals.GridSupervisor, [params]),
      supervisor(Fractals.EscapeTimeSupervisor, [params]),
      supervisor(Fractals.ColorizerSupervisor, [params]),
      supervisor(Fractals.OutputSupervisor, [params])
    ]
    supervise(children, strategy: :one_for_one)
  end
end
