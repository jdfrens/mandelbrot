defmodule Chunk do
  @moduledoc """
  Defines a chunk of work.
  """

  @type t :: %__MODULE__{
          number: integer,
          data: list,
          params: Fractals.Params.t()
        }

  defstruct number: nil, data: [], params: nil
end
