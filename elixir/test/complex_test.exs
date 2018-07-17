defmodule ComplexSpec do
  use ExUnit.Case, async: true

  import Complex, only: :macros

  describe ".parse" do
    test "parsing a complex number" do
      assert Complex.parse("1.1+2.2i") == cmplx(1.1, 2.2)
    end

    test "parsing negative numbers" do
      assert Complex.parse("-1.1+-2.2i") == cmplx(-1.1, -2.2)
    end
  end

  describe ".add" do
    test "adds" do
      z0 = cmplx(5.0, 12.0)
      z1 = cmplx(3.0, 2.0)
      assert Complex.add(z0, z1) == cmplx(8.0, 14.0)
    end
  end

  describe ".subtract" do
    test "subtracts" do
      z0 = cmplx(5.0, 12.0)
      z1 = cmplx(3.0, 2.0)
      assert Complex.subtract(z0, z1) == cmplx(2.0, 10.0)
    end
  end

  describe ".multiply" do
    test "multiplies" do
      z0 = cmplx(5.0, 12.0)
      z1 = cmplx(3.0, 2.0)
      assert Complex.multiply(z0, z1) == cmplx(-9.0, 46.0)
    end
  end

  describe ".square" do
    test "squares" do
      z1 = cmplx(3.0, 2.0)
      assert Complex.square(z1) == cmplx(5.0, 12.0)
    end
  end

  describe ".cube" do
    test "cubes" do
      z1 = cmplx(3.0, 2.0)
      assert Complex.cube(z1) == cmplx(-9.0, 46.0)
    end
  end

  describe ".magnitude" do
    test "squares and square roots" do
      assert Complex.magnitude(cmplx(3.0, 4.0)) == 5.0
    end
  end

  describe ".to_string" do
    test "uses a+bi format" do
      assert to_string(cmplx(3.2, 9.8)) == "3.2+9.8i"
    end

    test "handles negative numbers" do
      assert to_string(cmplx(-3.2, -9.8)) == "-3.2-9.8i"
    end
  end
end
