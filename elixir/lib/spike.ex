defmodule Spike do
  use GenStage

  def start_link() do
    GenStage.start_link(__MODULE__, :ok)
  end

  def init(:ok) do
    IO.puts("Spike init!!!")
    {:consumer, :irrelevant_state, subscribe_to: [Fractals.GridWorker]}
  end

  def handle_events(events, _from, state) do
    IO.puts("Spike handlin' events!!!!")
    IO.puts(length(events))
    {:noreply, [], state}
  end
end
