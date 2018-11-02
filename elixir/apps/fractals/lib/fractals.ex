defmodule Fractals do
  @moduledoc """
  The application.
  """

  alias Fractals.{
    GridWorker,
    Params,
    Reporters.Broadcaster
  }

  @unimplemented Application.get_env(:fractals, :unimplemented)

  @spec fractalize(Fractals.Params.t()) :: :ok
  def fractalize(params) do
    if unimplemented?(params.fractal) do
      Broadcaster.report(:skipping, params, reason: "fractal not implemented", from: self())
    else
      Broadcaster.report(:starting, params, from: self())
      GridWorker.work(Fractals.GridWorker, params)
    end

    :ok
  end

  @spec unimplemented?(Params.fractal_type()) :: boolean
  defp unimplemented?(fractal) do
    Enum.member?(@unimplemented, fractal)
  end
end
