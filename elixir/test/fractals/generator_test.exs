defmodule Fractals.GeneratorTest do

  use Pavlov.Case, async: true
	import Pavlov.Syntax.Expect

  import Complex, only: :macros

  describe ".generate_header" do
		it "spits out a PPM header including width and height" do
			options = %Fractals.Options{ size: %Fractals.Size{ width: 55, height: 99 } }
			expect Fractals.Generator.generate_header(options) |> to_eq ["P3", "55", "99", "255"]
		end
  end

  describe ".generate" do
    # more of a property test
    import Fractals.Generator, only: [ generate: 1 ]

		it "generates an image" do
			options       = %Fractals.Options{
				fractal:     :mandelbrot,
				color:       :blue,
				size:        %Fractals.Size{ width: 30, height: 20 },
				upper_left:  cmplx(-2.0,  1.0),
				lower_right: cmplx( 1.0, -1.0)
																 }

			ppm = generate(options) |> Enum.to_list
			# 4 lines of header + 30 columns * 20 rows
			expect Enum.count(ppm) |> to_eq 604
		end
  end

	let :next do
		fn z -> z + 1 end
	end

  describe ".fractal_iterate" do
    import Fractals.Generator, only: [ fractal_iterate: 3 ]

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
    import Fractals.Generator, only: [ in_or_out: 1 ]

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
