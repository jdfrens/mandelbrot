defmodule Mandelbrot.Fractal.Test do

  use Pavlov.Case, async: true
	import Pavlov.Syntax.Expect

  describe ".generate_header" do
		it "spits out a PPM header including width and height" do
			options = %Mandelbrot.Options{ size: %Mandelbrot.Size{ width: 55, height: 99 } }
			expect Mandelbrot.Fractal.generate_header(options) |> to_eq ["P3", "55", "99", "255"]
		end
  end

  describe ".generate" do
    # more of a property test
    import Mandelbrot.Fractal, only: [ generate: 1 ]

		it "generates an image" do
			options       = %Mandelbrot.Options{
				fractal:     :mandelbrot,
				color:       :blue,
				size:        %Mandelbrot.Size{ width: 30, height: 20 },
				upper_left:  %Complex{ real: -2.0, imag:  1.0 },
				lower_right: %Complex{ real:  1.0, imag: -1.0 }
																 }

			ppm = Mandelbrot.Fractal.generate(options) |> Enum.to_list
			# 4 lines of header + 30 columns * 20 rows
			expect Enum.count(ppm) |> to_eq 604
		end
  end

  describe ".fractal_iterate" do
    import Mandelbrot.Fractal, only: [ fractal_iterate: 3 ]

		let :next do
			fn z -> z + 1 end
		end

		it "stops due to cutoff" do
			cutoff = fn z -> z < 0 end
			expect fractal_iterate(next, cutoff, 1) |> to_eq { 1, 0 }
		end

		it "stops later because of cutoff" do
			cutoff = fn z -> z < 128 end
			expect fractal_iterate(next, cutoff, 1) |> to_eq { 128, 127 }
		end

		it "stops because of the number of iterations" do
			cutoff = fn z -> z < 500 end
			expect fractal_iterate(next, cutoff, 1) |> to_eq { 256, 255 }
		end
  end

  describe ".in_or_out" do
    import Mandelbrot.Fractal, only: [ in_or_out: 1 ]

		it "is inside when iterations are greater than or equal to 255" do
			expect in_or_out({ :meh, 538 }) |> to_eq { :inside,  :meh, 538 }
			expect in_or_out({ :meh, 256 }) |> to_eq { :inside,  :meh, 256 }
			expect in_or_out({ :meh, 255 }) |> to_eq { :inside,  :meh, 255 }
		end

		it "is outside when iterations are less than 255" do
			expect in_or_out({ :meh, 254 }) |> to_eq { :outside,  :meh, 254 }
			expect in_or_out({ :meh,  34 }) |> to_eq { :outside,  :meh,  34 }
		end
  end

end
