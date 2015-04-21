defmodule Complex do
  defstruct [ :real, :imag ]

  def parse(str) do
    [_, real, imag] = Regex.run(~r/([-]?\d+\.\d+)\+([-]?\d+\.\d+)i/, str)
    %Complex{
      real: String.to_float(real),
      imag: String.to_float(imag)
    }
  end

end
