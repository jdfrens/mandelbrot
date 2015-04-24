defmodule Mandelbrot.NextFunction.Test do

  use ExUnit.Case, async: true

  import Mandelbrot.NextFunction, only: [ mandelbrot_next: 1, julia_next: 1, burning_ship_next: 1 ]

  test "mandelbrot_next" do
    z = %Complex{ real: 3.0, imag: 2.0 }
    c = %Complex{ real: 1.0, imag: 1.0 }
    assert %Complex{ real: 6.0, imag: 13.0 } == mandelbrot_next(c).(z)
  end

  test "julia_next" do
    z = %Complex{ real: 3.0, imag: 2.0 }
    c = %Complex{ real: 1.0, imag: 1.0 }
    assert %Complex{ real: 6.0, imag: 13.0 } == julia_next(c).(z)
  end

  test "burning_ship_next" do
    z = %Complex{ real: 3.0, imag: 2.0 }
    c = %Complex{ real: 1.0, imag: 1.0 }
    assert %Complex{ real: 6.0, imag: -11.0 } == burning_ship_next(c).(z)
  end

  test "burn" do
    import Mandelbrot.NextFunction, only: [ burn: 1 ]

    assert %Complex{ real: 5.0, imag: -3.0 } == burn(%Complex{ real:  5.0, imag:  3.0 })
    assert %Complex{ real: 5.0, imag: -3.0 } == burn(%Complex{ real:  5.0, imag: -3.0 })
    assert %Complex{ real: 5.0, imag: -3.0 } == burn(%Complex{ real: -5.0, imag:  3.0 })
    assert %Complex{ real: 5.0, imag: -3.0 } == burn(%Complex{ real: -5.0, imag: -3.0 })
  end

end
