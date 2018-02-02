defmodule Fractals.EscapeTime.BurningShipTest do
  use ExUnit.Case, aysnc: true

  import Complex

  alias Fractals.EscapeTime.BurningShip

  describe ".iterator" do
    test "burns and squares and adds" do
      assert BurningShip.iterator(cmplx(3.0, 2.0), cmplx(1.0, 1.0)) == cmplx(6.0, -11.0)
    end
  end

  describe ".burn" do
    test "negates the imaginary component" do
      assert BurningShip.burn(cmplx(5.0, 3.0)) == cmplx(5.0, -3.0)
    end

    test "keeps the imaginary component negative" do
      assert BurningShip.burn(cmplx(5.0, -3.0)) == cmplx(5.0, -3.0)
    end

    test "uses the absolute value of the real component" do
      assert BurningShip.burn(cmplx(-5.0, 3.0)) == cmplx(5.0, -3.0)
    end
  end
end
