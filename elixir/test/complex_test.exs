defmodule Complex.Test do

  use ExUnit.Case, async: true

  test "parse" do
    assert %Complex{ real:  1.1, imag:  2.2 } == Complex.parse("1.1+2.2i")
    assert %Complex{ real: -1.1, imag: -2.2 } == Complex.parse("-1.1+-2.2i")
  end

end
