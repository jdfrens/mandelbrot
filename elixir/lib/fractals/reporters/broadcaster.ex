defmodule Fractals.Reporters.Broadcaster do
  @moduledoc """
  Broadcasts messages to reporters.
  """

  alias Fractals.Reporters.{FilenameCountdown, Stdout}

  @reporters [Stdout, FilenameCountdown]

  @spec report(atom, Params.t(), keyword) :: :ok
  def report(tag, params, opts \\ []) do
    Enum.each(@reporters, fn x ->
      GenServer.cast(x, {tag, params, opts})
    end)
  end
end
