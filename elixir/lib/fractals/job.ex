defmodule Fractals.Job do
  use Supervisor

  def start_link do
    Supervisor.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    children = [
      worker(Progress, [[:generate_chunk, :escape_chunk, :colorize_chunk, :write_chunk]]),
      supervisor(Fractals.GridSupervisor, []),
      supervisor(Fractals.EscapeTimeSupervisor, []),
      supervisor(Fractals.ColorizerSupervisor, []),
      supervisor(Fractals.OutputSupervisor, []),
      supervisor(Fractals.ConversionSupervisor, [])
    ]
    supervise(children, strategy: :one_for_one)
  end
end
