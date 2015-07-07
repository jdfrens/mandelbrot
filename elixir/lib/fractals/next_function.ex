defmodule Fractals.NextFunction do

  def next_function(grid_point, options) do
    case options.fractal do
      :burningship -> burning_ship_next(grid_point)
      :mandelbrot  -> mandelbrot_next(grid_point)
      :julia       -> julia_next(options.c)
      :newton      -> newton()
      :nova        -> nova(grid_point)
    end
  end

  import Complex, only: [ add: 2, subtract: 2,
                          multiply: 2, square: 1, cube: 1,
                          divide: 2 ]

  def mandelbrot_next(c) do
    fn (z) -> z |> square() |> add(c) end
  end

  def julia_next(c) do
    fn (z) -> z |> square() |> add(c) end
  end

  def burning_ship_next(c) do
    fn (z) -> burn(z) |> square() |> add(c) end
  end

  def burn(%Complex{ real: real, imag: imag }) do
    # FIXME: not sure why I need to negate imag since Wikipedia doesn't
    %Complex{ real: abs(real), imag: -1 * abs(imag) }
  end

  def newton do
    fn (z) -> divide(p(z), p_prime(z)) end
  end

  def p(z) do
    z |> cube() |> subtract(%Complex{ real: -1, imag: 0 })
  end

  def p_prime(z) do
    z |> square() |> multiply(%Complex{ real: 3.0, imag: 0.0 })
  end

  # FIXME: not implementing for now; z0^z1 is what, exactly?
  # z, r, and p are all Complex:
  # z - r * (z ** p - 1) / (p * z ** (p - 1)) + c
  def nova(_options) do
    fn z -> z end
  end

end
