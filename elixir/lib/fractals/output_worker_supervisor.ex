defmodule Fractals.OutputWorkerSupervisor do
  use DynamicSupervisor

  def start_link(_) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  def new_worker(options \\ []) do
    next_stage =
      Keyword.get(options, :next_stage, fn params ->
        Fractals.Params.close(params)
        Fractals.ConversionWorker.convert(params)
      end)
    name = Keyword.get(options, :name)
    child_spec = {Fractals.OutputWorker, {next_stage, name}}

    DynamicSupervisor.start_child(__MODULE__, child_spec)
  end

end
