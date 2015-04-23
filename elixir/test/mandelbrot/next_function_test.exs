defmodule Mandelbrot.NextFunction.Test do

  use ExUnit.Case, async: true

  test "mandelbrot_next" do
    z = %Complex{ real: 3.0, imag: 2.0 }
    c = %Complex{ real: 1.0, imag: 1.0 }
    assert %Complex{ real: 6.0, imag: 13.0 } == Mandelbrot.NextFunction.mandelbrot_next(z, c)
  end

  test "burning_ship_next" do
    z = %Complex{ real: 3.0, imag: 2.0 }
    c = %Complex{ real: 1.0, imag: 1.0 }
    assert %Complex{ real: 6.0, imag: -11.0 } == Mandelbrot.NextFunction.burning_ship_next(z, c)
  end

  test "burn" do
    import Mandelbrot.NextFunction, only: [ burn: 1 ]

    assert %Complex{ real: 5.0, imag: -3.0 } == burn(%Complex{ real:  5.0, imag:  3.0 })
    assert %Complex{ real: 5.0, imag: -3.0 } == burn(%Complex{ real:  5.0, imag: -3.0 })
    assert %Complex{ real: 5.0, imag: -3.0 } == burn(%Complex{ real: -5.0, imag:  3.0 })
    assert %Complex{ real: 5.0, imag: -3.0 } == burn(%Complex{ real: -5.0, imag: -3.0 })
  end

end
