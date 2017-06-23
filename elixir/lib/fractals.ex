defmodule Fractals do
  use Application

  @progress_measures [:generate_chunk, :escape_chunk, :colorize_chunk, :write_chunk]
  @unimplemented Application.get_env(:fractals, :unimplemented)

  def fractalize(params) do
    # TODO: call start process instead
    unless Enum.member?(@unimplemented, params.fractal) do
      Fractals.GridWorker.work(Fractals.GridWorker, params)
    end
  end

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    children = [
      worker(Progress, [@progress_measures]),
      # TODO: start process
      supervisor(Fractals.GridSupervisor, []),
      supervisor(Fractals.EscapeTimeSupervisor, []),
      supervisor(Fractals.ColorizerSupervisor, []),
      supervisor(Fractals.OutputSupervisor, []),
      supervisor(Fractals.ConversionSupervisor, []),
      # TODO: end process
    ]
    Supervisor.start_link(children, strategy: :one_for_one)
  end
end

# TODO: the start process
# 1. set params.start_timestamp
# 2. invokes the first real process

# TODO: the end process
# 1. set params.end_timestamp
# 2. sends "ALL DONE!" message
