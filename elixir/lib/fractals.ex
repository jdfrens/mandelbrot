defmodule Fractals do
  use Application

  @progress_measures [:generate_chunk, :escape_chunk, :colorize_chunk, :write_chunk]
  @unimplemented Application.get_env(:fractals, :unimplemented)

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    staged = [
      worker(Progress, [@progress_measures]),
      # TODO: start process
      worker(Fractals.GridWorker, []),
      worker(Fractals.EscapeTimeWorker, []),
      worker(Fractals.ColorizerWorker, []),
      worker(Fractals.OutputManager, [])
      # TODO: end process
    ]

    unstaged = [
      worker(Fractals.Colorizer.Random, []),
      supervisor(Fractals.OutputWorkerSupervisor, []),
      worker(Fractals.ConversionWorker, [])
    ]

    Supervisor.start_link(staged ++ unstaged, strategy: :one_for_one)
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
