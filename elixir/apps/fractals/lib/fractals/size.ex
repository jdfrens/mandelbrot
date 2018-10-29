defmodule Fractals.Size do
  @moduledoc """
  Structure to keep track of width and height
  """

  @type t :: %__MODULE__{
          width: integer,
          height: integer
        }

  defstruct [:width, :height]
end
