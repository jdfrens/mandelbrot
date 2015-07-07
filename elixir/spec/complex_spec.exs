defmodule ComplexSpec do

  use ESpec

  import Complex, only: :macros

	describe ".parse" do
		it "parses" do
			expect(Complex.parse("1.1+2.2i")).to eq(cmplx( 1.1,  2.2))
		end
		it "parses negative numbers" do
			expect(Complex.parse("-1.1+-2.2i")).to eq(cmplx(-1.1, -2.2))
		end
	end

	let :z0, do: cmplx(5.0, 12.0)
	let :z1, do: cmplx(3.0,  2.0)

	describe ".add" do
		it "adds" do
			expect(Complex.add(z0, z1)).to eq(cmplx(8.0, 14.0))
		end
	end

  describe ".subtract" do
		it "subtracts" do
			expect(Complex.subtract(z0, z1)).to eq(cmplx(2.0, 10.0))
		end
  end

  describe ".multiply" do
		it "multiplies" do
			expect(Complex.multiply(z0, z1)).to eq(cmplx(-9.0, 46.0))
		end
  end

  describe ".square" do
		it "squares" do
			expect(Complex.square(z1)).to eq(cmplx(5.0, 12.0))
		end
	end

  describe ".cube" do
		it "cubes" do
			expect(Complex.cube(z1)).to eq(cmplx(-9.0, 46.0))
		end
  end

  describe ".magnitude" do
    it "squares and square roots" do
			expect(Complex.magnitude(cmplx(3.0, 4.0))).to eq(5.0)
		end
  end

end
