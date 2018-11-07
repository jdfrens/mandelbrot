defmodule StageEngine do
  @moduledoc """
  Documentation for StageEngine.
  """

  alias StageEngine.GridWorker

  @spec generate(Fractals.Params.t()) :: :ok
  def generate(params) do
    GridWorker.work(StageEngine.GridWorker, params)
  end
end
