defmodule Fractals.ColorizerSupervisor do
  use Supervisor

  def start_link(options) do
    Supervisor.start_link(__MODULE__, options)
  end

  def init(options) do
    children = [
      worker(Fractals.ColorizerWorker, [options]),
      worker(Fractals.Colorizer.Random, [:ok])
    ]
    supervise(children, strategy: :one_for_one)
  end
end
