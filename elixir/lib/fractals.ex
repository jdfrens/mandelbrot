defmodule Fractals do
  use Application

  @progress_measures [:generate_chunk, :escape_chunk, :colorize_chunk, :write_chunk]
  @unimplemented Application.get_env(:fractals, :unimplemented)

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    grid = worker(Fractals.GridWorker, [])
    escape_time = worker(Fractals.EscapeTimeWorker, [])
    # worker(Fractals.ColorizerWorker, [])
    # worker(Fractals.Colorizer.Random, [])
    # worker(Fractals.OutputManager, [])
    # supervisor(Fractals.OutputWorkerSupervisor, [])
    # worker(Fractals.ConversionWorker, [])
    spike = worker(Spike, [])

    children = [
      worker(Progress, [@progress_measures]),
      # TODO: start process
      grid,
      escape_time,
      # TODO: end process
      spike
    ]
    Supervisor.start_link(children, strategy: :one_for_one)
  end

  def fractalize(params) do
    # TODO: call start process instead
    unless implemented?(params.fractal) do
      Fractals.GridWorker.work(Fractals.GridWorker, params)
    end
  end

  defp implemented?(fractal) do
    Enum.member?(@unimplemented, fractal)
  end
end

# TODO: the start process
# 1. set params.start_timestamp
# 2. invokes the first real process

# TODO: the end process
# 1. set params.end_timestamp
# 2. sends "ALL DONE!" message
