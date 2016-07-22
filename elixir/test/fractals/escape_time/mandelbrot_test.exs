defmodule Fractals.EscapeTime.MandelbrotTest do
  use ExUnit.Case, async: true

  import Complex

  alias Fractals.EscapeTime.Mandelbrot

  describe ".iterator" do
    test "it squares and adds" do
      z = cmplx(3.0, 2.0)
      c = cmplx(1.0, 1.0)
      assert Mandelbrot.iterator(z, c) == cmplx(6.0, 13.0)
    end
  end
end
