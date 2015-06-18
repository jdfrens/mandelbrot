defmodule Complex.Test do

  use Pavlov.Case, async: true
  import Pavlov.Syntax.Expect

	describe ".parse" do
		it "parses" do
			expect Complex.parse("1.1+2.2i") |> to_eq %Complex{ real:  1.1, imag:  2.2 }
		end
		it "parses negative numbers" do
			expect Complex.parse("-1.1+-2.2i") |> to_eq %Complex{ real: -1.1, imag: -2.2 }
		end
	end

	describe ".add" do
		it "adds" do
			z0 = %Complex{ real: 5.0, imag: 12.0 }
			z1 = %Complex{ real: 3.0, imag:  2.0 }
			expect Complex.add(z0, z1) |> to_eq %Complex{ real: 8.0, imag: 14.0 }
		end
	end

  describe ".subtract" do
		it "subtracts" do
			z0 = %Complex{ real: 5.0, imag: 12.0 }
			z1 = %Complex{ real: 3.0, imag:  2.0 }
			expect Complex.subtract(z0, z1) |> to_eq %Complex{ real: 2.0, imag: 10.0 }
		end
  end

  describe ".multiply" do
		it "multiplies" do
			z0 = %Complex{ real: 5.0, imag: 12.0 }
			z1 = %Complex{ real: 3.0, imag:  2.0 }
			expect Complex.multiply(z0, z1) |> to_eq %Complex{ real: -9.0, imag: 46.0 }
		end
  end

  describe ".square" do
		it "squares" do
			expect Complex.square(%Complex{ real: 3.0, imag: 2.0 })
			|> to_eq %Complex{ real: 5.0, imag: 12.0 }
		end
	end

  describe ".cube" do
		it "cubes" do
			expect Complex.cube(%Complex{ real: 3.0, imag: 2.0 })
			|> to_eq %Complex{ real: -9.0, imag: 46.0 }
		end
  end

  describe ".magnitude" do
    it "squares and square roots" do
			expect Complex.magnitude(%Complex{ real: 3.0, imag: 4.0 }) |> to_eq 5.0
		end
  end

end
