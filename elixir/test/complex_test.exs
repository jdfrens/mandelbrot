defmodule Complex.Test do

  use ExUnit.Case, async: true

  test "parse" do
    assert %Complex{ real:  1.1, imag:  2.2 } == Complex.parse("1.1+2.2i")
    assert %Complex{ real: -1.1, imag: -2.2 } == Complex.parse("-1.1+-2.2i")
  end

  test "add" do
    z0 = %Complex{ real: 5.0, imag: 12.0 }
    z1 = %Complex{ real: 3.0, imag:  2.0 }
    assert %Complex{ real: 8.0, imag: 14.0 } == Complex.add(z0, z1)
  end

  test "subtract" do
    z0 = %Complex{ real: 5.0, imag: 12.0 }
    z1 = %Complex{ real: 3.0, imag:  2.0 }
    assert %Complex{ real: 2.0, imag: 10.0 } == Complex.subtract(z0, z1)
  end

  test "multiply" do
    z0 = %Complex{ real: 5.0, imag: 12.0 }
    z1 = %Complex{ real: 3.0, imag:  2.0 }
    assert %Complex{ real: -9.0, imag: 46.0 } == Complex.multiply(z0, z1)
  end

  test "square" do
    assert %Complex{ real: 5.0, imag: 12.0 } == Complex.square(%Complex{ real: 3.0, imag: 2.0 })
  end

  test "cube" do
    assert %Complex{ real: -9.0, imag: 46.0 } == Complex.cube(%Complex{ real: 3.0, imag: 2.0 })
  end

  test "magnitude" do
    assert 5.0 = Complex.magnitude(%Complex{ real: 3.0, imag: 4.0 })
  end

end
