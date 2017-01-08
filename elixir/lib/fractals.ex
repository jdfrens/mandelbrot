defmodule Fractals do
  use Application

  @progress_measures [:generate_chunk, :escape_chunk, :colorize_chunk, :write_chunk]

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(Progress, [@progress_measures]),
      supervisor(Fractals.GridSupervisor, []),
      supervisor(Fractals.EscapeTimeSupervisor, []),
      supervisor(Fractals.ColorizerSupervisor, []),
      supervisor(Fractals.OutputSupervisor, []),
      supervisor(Fractals.ConversionSupervisor, [])
    ]
    Supervisor.start_link(children, strategy: :one_for_one)
  end
end
