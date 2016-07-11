defmodule Fractals.GridSupervisor do
  use Supervisor

  def start_link(params) do
    Supervisor.start_link(__MODULE__, params)
  end

  def init(params) do
    children = [
      worker(Fractals.GridWorker, [params])
    ]
    supervise(children, strategy: :one_for_one)
  end
end
