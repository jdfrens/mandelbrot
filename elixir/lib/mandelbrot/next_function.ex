defmodule Mandelbrot.NextFunction do

  def next_function(options) do
    case options.fractal do
      :mandelbrot -> &mandelbrot_next/2
    end
  end

  def mandelbrot_next(z, c) do
    z
    |> Complex.square()
    |> Complex.add(c)
  end

end
