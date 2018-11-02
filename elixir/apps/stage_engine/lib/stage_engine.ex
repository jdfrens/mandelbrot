defmodule StageEngine do
  @moduledoc """
  Documentation for StageEngine.
  """

  alias Fractals.GridWorker

  def generate(params) do
    GridWorker.work(Fractals.GridWorker, params)
  end
end
