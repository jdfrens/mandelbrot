defmodule Fractals.JobSupervisor do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, nil, name: __MODULE__)
  end

  def init(_) do
    supervise([worker(Fractals.Job, [])], strategy: :simple_one_for_one)
  end

  def start_child(params) do
    Supervisor.start_child(__MODULE__, [params])
  end
end
