defmodule Fractals.EscapeTime.MandelbrotTest do
  use ExUnit.Case, async: true

  alias Fractals.EscapeTime.Mandelbrot

  describe ".iterator" do
    test "it squares and adds" do
      z = Complex.new(3.0, 2.0)
      c = Complex.new(1.0, 1.0)
      assert Mandelbrot.iterator(z, c) == Complex.new(6.0, 13.0)
    end
  end
end
