defmodule StageEngine do
  @moduledoc """
  Documentation for StageEngine.
  """

  alias StageEngine.GridWorker

  def generate(params) do
    GridWorker.work(StageEngine.GridWorker, params)
  end
end
