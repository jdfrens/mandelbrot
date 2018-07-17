defmodule Fractals.Output.OutputState do
  @moduledoc """
  Keeps track of the next chunk that needs to be output.  Chunks that cannot be output yet are kept in a cache.
  """
  defstruct next_number: 0, cache: %{}
end
