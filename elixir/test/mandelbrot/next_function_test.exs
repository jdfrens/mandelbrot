defmodule Mandelbrot.NextFunction.Test do

  use Pavlov.Case, async: true
	import Pavlov.Syntax.Expect

  describe ".mandelbrot_next" do
		import Mandelbrot.NextFunction, only: [ mandelbrot_next: 1 ]

		it "squares and adds" do
			z = %Complex{ real: 3.0, imag: 2.0 }
			c = %Complex{ real: 1.0, imag: 1.0 }
			expect(mandelbrot_next(c).(z)) |> to_eq %Complex{ real: 6.0, imag: 13.0 }
		end
  end

  describe ".julia_next" do
		import Mandelbrot.NextFunction, only: [ julia_next: 1 ]

		it "squares and adds" do
			z = %Complex{ real: 3.0, imag: 2.0 }
			c = %Complex{ real: 1.0, imag: 1.0 }
			expect julia_next(c).(z) |> to_eq %Complex{ real: 6.0, imag: 13.0 }
		end
  end

  describe ".burning_ship_next" do
		import Mandelbrot.NextFunction, only: [ burning_ship_next: 1 ]

		it "burns and squares and adds" do
			z = %Complex{ real: 3.0, imag: 2.0 }
			c = %Complex{ real: 1.0, imag: 1.0 }
			burning_ship_next(c).(z) |> to_eq %Complex{ real: 6.0, imag: -11.0 }
		end

		describe ".burn" do
			import Mandelbrot.NextFunction, only: [ burn: 1 ]

			it "negates the imaginary component" do
				expect burn(%Complex{ real:  5.0, imag:  3.0 })
				|> to_eq %Complex{ real: 5.0, imag: -3.0 }
			end
			it "keeps the imaginary component negative" do
				burn(%Complex{ real:  5.0, imag: -3.0 })
				|> to_eq %Complex{ real: 5.0, imag: -3.0 }
			end
			it "uses the absolute value of the real component" do
				burn(%Complex{ real: -5.0, imag:  3.0 })
				|> to_eq %Complex{ real: 5.0, imag: -3.0 }
			end
		end
  end

  describe ".newton" do

  end
end
