defmodule Mandelbrot.NextFunction do

  def next_function(options) do
    case options.fractal do
      :burningship -> &burning_ship_next/2
      :mandelbrot  -> &mandelbrot_next/2
    end
  end

  import Complex, only: [ square: 1, add: 2 ]

  def mandelbrot_next(z, c) do
    z |> square() |> add(c)
  end

  def burning_ship_next(z, c) do
    burn(z) |> square() |> add(c)
  end

  def burn(%Complex{ real: real, imag: imag }) do
    # FIXME: not sure why I need to negate imag since Wikipedia doesn't
    %Complex{ real: abs(real), imag: -1 * abs(imag) }
  end

end
