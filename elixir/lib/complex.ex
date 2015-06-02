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
    magnitude_squared(z) |> :math.sqrt
  end

  def magnitude_squared(%Complex{real: real, imag: imag}) do
    real * real + imag * imag
  end

  def add(%Complex{real: a, imag: b}, %Complex{real: c, imag: d}) do
    %Complex { real: a + c, imag: b + d }
  end

  def subtract(%Complex{real: a, imag: b}, %Complex{real: c, imag: d}) do
    %Complex { real: a - c, imag: b - d }
  end

  def multiply(%Complex{real: a, imag: b}, %Complex{real: c, imag: d}) do
    %Complex{
      real: a * c - b * d,
      imag: a * d + b * c
    }
  end

  def square(%Complex{real: a, imag: b}) do
    %Complex{
      real: a * a - b * b,
      imag: 2 * a * b
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
