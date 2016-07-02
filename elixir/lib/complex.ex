defmodule Complex do
  @moduledoc """
  Module for complex numbers.

  cf. http://erlangcentral.org/wiki/index.php/Complex_Numbers
  """

  defstruct [:real, :imag]

  @type t :: %Complex{real: float, imag: float}

  defmacro cmplx(real, imag) do
    quote do
      %Complex{real: unquote(real), imag: unquote(imag)}
    end
  end

  defmacro zero do
    quote do
      %Complex{real: 0.0, imag: 0.0}
    end
  end

  @spec parse(String.t) :: Complex.t
  def parse(str) do
    [_, real, imag] = Regex.run(~r/([-]?\d+\.\d+)\+([-]?\d+\.\d+)i/, str)
    %Complex{
      real: String.to_float(real),
      imag: String.to_float(imag)
    }
  end

  @spec magnitude(Complex.t) :: float
  def magnitude(z) do
    z |> magnitude_squared |> :math.sqrt
  end

  @spec magnitude_squared(Complex.t) :: float
  def magnitude_squared(%Complex{real: real, imag: imag}) do
    real * real + imag * imag
  end

  @spec add(Complex.t, Complex.t) :: Complex.t
  def add(%Complex{real: a, imag: b}, %Complex{real: c, imag: d}) do
    %Complex{real: a + c, imag: b + d}
  end

  @spec subtract(Complex.t, Complex.t) :: Complex.t
  def subtract(%Complex{real: a, imag: b}, %Complex{real: c, imag: d}) do
    %Complex{real: a - c, imag: b - d}
  end

  @spec multiply(Complex.t, Complex.t) :: Complex.t
  def multiply(%Complex{real: a, imag: b}, %Complex{real: c, imag: d}) do
    %Complex{
      real: a * c - b * d,
      imag: a * d + b * c
    }
  end

  @spec square(Complex.t) :: Complex.t
  def square(%Complex{real: a, imag: b}) do
    %Complex{
      real: a * a - b * b,
      imag: 2 * a * b
    }
  end

  @spec cube(Complex.t) :: Complex.t
  def cube(z) do
    z |> square() |> multiply(z)
  end

  @spec divide(Complex.t, Complex.t) :: Complex.t
  def divide(z0, z1) do
    denominator = z1.real * z1.real + z1.imag * z1.imag
    %Complex{
      real: (z0.real * z1.real + z0.imag * z1.imag) / denominator,
      imag: (z0.imag * z1.real - z0.real * z1.imag) / denominator
    }
  end
end

defimpl String.Chars, for: Complex do
  def to_string(complex) do
    sign = if complex.imag < 0, do: "", else: "+"
    "#{complex.real}#{sign}#{complex.imag}i"
  end
end
