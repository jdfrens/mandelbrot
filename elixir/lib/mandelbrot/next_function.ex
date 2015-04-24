defmodule Mandelbrot.NextFunction do

  def next_function(grid_point, options) do
    case options.fractal do
      :burningship -> burning_ship_next(grid_point)
      :mandelbrot  -> mandelbrot_next(grid_point)
      :julia       -> julia_next(options.c)
    end
  end

  import Complex, only: [ square: 1, add: 2 ]

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

end
