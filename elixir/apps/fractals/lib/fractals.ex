defmodule Fractals do
  @moduledoc """
  The application.
  """

  alias Fractals.{Params, Reporters.Broadcaster}

  @unimplemented Application.get_env(:fractals, :unimplemented)

  @spec fractalize(Fractals.Params.t(), module()) :: :ok
  def fractalize(params, engine) do
    if unimplemented?(params.fractal) do
      Broadcaster.report(:skipping, params, reason: "fractal not implemented", from: self())
    else
      Broadcaster.report(:starting, params, from: self())
      engine.generate(params)
    end

    :ok
  end

  @spec unimplemented?(Params.fractal_type()) :: boolean
  defp unimplemented?(fractal) do
    Enum.member?(@unimplemented, fractal)
  end
end
