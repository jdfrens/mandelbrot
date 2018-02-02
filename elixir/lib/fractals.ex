defmodule Fractals do
  use Application

  @progress_measures [:generate_chunk, :escape_chunk, :colorize_chunk, :write_chunk]
  @unimplemented Application.get_env(:fractals, :unimplemented)

  def start(_type, _args) do
    import Supervisor.Spec, warn: false

    staged = [
      {Progress, scopes: @progress_measures},
      # TODO: start process
      Fractals.GridWorker,
      Fractals.EscapeTimeWorker,
      Fractals.ColorizerWorker,
      Fractals.OutputManager
      # TODO: end process
    ]

    unstaged = [
      Fractals.Colorizer.Random,
      Fractals.OutputWorkerSupervisor,
      Fractals.ConversionWorker
    ]

    Supervisor.start_link(staged ++ unstaged, strategy: :one_for_one)
  end

  def fractalize(params) do
    send(params.source_pid, {:starting, self(), params})

    if unimplemented?(params.fractal) do
      # TODO: extract a common library for these notifications
      send(params.source_pid, {:skipping, self(), params, "not implemented"})
    else
      Fractals.GridWorker.work(Fractals.GridWorker, params)
    end
  end

  defp unimplemented?(fractal) do
    Enum.member?(@unimplemented, fractal)
  end
end

# TODO: the start process
# 1. set params.start_timestamp
# 2. invokes the first real process

# TODO: the end process
# 1. set params.end_timestamp
# 2. sends "ALL DONE!" message
