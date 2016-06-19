defmodule Fractals.Job do
  use Supervisor

  def start_link(options) do
    Supervisor.start_link(__MODULE__, [options])
  end

  def init(options) do
    children = [
      supervisor(Fractals.GridSupervisor, [options]),
      supervisor(Fractals.EscapeTimeSupervisor, [options]),
      supervisor(Fractals.ColorizerSupervisor, [options]),
      supervisor(Fractals.OutputSupervisor, [options])
    ]
    supervise(children, strategy: :one_for_one)
  end
end
