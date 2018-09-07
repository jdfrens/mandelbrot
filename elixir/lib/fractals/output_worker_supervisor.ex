defmodule Fractals.OutputWorkerSupervisor do
  @moduledoc false

  use DynamicSupervisor

  alias Fractals.{ConversionWorker, OutputWorker, Params}

  # Client

  @spec start_link(any) :: Supervisor.on_start()
  def(start_link(_)) do
    DynamicSupervisor.start_link(__MODULE__, :ok, name: __MODULE__)
  end

  # Server

  def init(:ok) do
    DynamicSupervisor.init(strategy: :one_for_one)
  end

  @spec new_worker(keyword) :: Supervisor.on_start_child()
  def new_worker(options \\ []) do
    next_stage =
      Keyword.get(options, :next_stage, fn params ->
        Params.close(params)
        ConversionWorker.convert(params)
      end)

    name = Keyword.get(options, :name)
    child_spec = {OutputWorker, {next_stage, name}}

    DynamicSupervisor.start_child(__MODULE__, child_spec)
  end
end
