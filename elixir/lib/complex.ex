defmodule Complex do
  defstruct [ :real, :imag ]

  def parse(str) do
    [_, real, imag] = Regex.run(~r/([-]?\d+\.\d+)\+([-]?\d+\.\d+)i/, str)
    %Complex{
      real: String.to_float(real),
      imag: String.to_float(imag)
    }
  end

  def magnitude(z) do
    :math.sqrt(z.real * z.real + z.imag * z.imag)
  end

  def add(z0, z1) do
    %Complex { real: z0.real + z1.real, imag: z0.imag + z1.imag }
  end

  def subtract(z0, z1) do
    %Complex { real: z0.real - z1.real, imag: z0.imag - z1.imag }
  end

  def multiply(z0, z1) do
    %Complex{
      real: z0.real * z1.real - z0.imag * z1.imag,
      imag: z0.real * z1.imag + z0.imag * z1.real
    }
  end

  def square(z) do
    %Complex{
      real: z.real * z.real - z.imag * z.imag,
      imag: 2 * z.real * z.imag
    }
  end

  def cube(z) do
    z |> square() |> multiply(z)
  end

  def divide(z0, z1) do
    denominator = z1.real * z1.real + z1.imag * z1.imag
    %Complex{
      real: (z0.real * z1.real + z0.imag * z1.imag) / denominator,
      imag: (z0.imag * z1.real - z0.real * z1.imag) / denominator
    }
  end

end
