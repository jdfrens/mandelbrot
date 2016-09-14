defmodule Fractals.ColorizerSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    children = [
      worker(Fractals.ColorizerWorker, []),
      worker(Fractals.Colorizer.Random, [])
    ]
    supervise(children, strategy: :one_for_one)
  end
end
