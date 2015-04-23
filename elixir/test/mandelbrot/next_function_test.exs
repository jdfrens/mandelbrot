defmodule Mandelbrot.NextFunction.Test do

  use ExUnit.Case, async: true

  test "mandelbrot_next" do
    z = %Complex{ real: 3.0, imag: 2.0 }
    c = %Complex{ real: 1.0, imag: 1.0 }
    assert %Complex{ real: 6.0, imag: 13.0 } == Mandelbrot.NextFunction.mandelbrot_next(z, c)
  end

end
