defmodule Fractals.FractalSpec do

  use ESpec

  describe ".header" do
    it "spits out a PPM header including width and height" do
      options = %Fractals.Options{ size: %Fractals.Size{ width: 55, height: 99 } }
      expect(Fractals.Generator.header(options)).to eq(["P3", "55", "99", "255"])
    end
  end

  # more of a property test
  describe ".generate" do
    let :options do
      %Fractals.Options{
        fractal:     :mandelbrot,
        color:       :blue,
        size:        %Fractals.Size{ width: 30, height: 20 },
        upper_left:  %Complex{ real: -2.0, imag:  1.0 },
        lower_right: %Complex{ real:  1.0, imag: -1.0 }
      }
    end

    it "generates an image" do
      ppm = Fractals.Generator.generate(options) |> Enum.to_list
      # 4 lines of header + 30 columns * 20 rows
      expect(Enum.count(ppm)).to eq(604)
    end
  end

  describe ".iterate" do
    import Fractals.Generator, only: [ iterate: 3 ]

    let :next do
      fn z -> z + 1 end
    end

    it "stops due to cutoff" do
      cutoff = fn z -> z < 0 end
      expect(iterate(next, cutoff, 1)).to eq({ 1, 0 })
    end

    it "stops later because of cutoff" do
      cutoff = fn z -> z < 128 end
      expect(iterate(next, cutoff, 1)).to eq({ 128, 127 })
    end

    it "stops because of the number of iterations" do
      cutoff = fn z -> z < 500 end
      expect(iterate(next, cutoff, 1)).to eq({ 256, 255 })
    end
  end

  describe ".in_or_out" do
    import Fractals.Generator, only: [ in_or_out: 1 ]

    it "is inside when iterations are greater than or equal to 255" do
      expect(in_or_out({ :meh, 538 })).to eq({ :inside,  :meh, 538 })
      expect(in_or_out({ :meh, 256 })).to eq({ :inside,  :meh, 256 })
      expect(in_or_out({ :meh, 255 })).to eq({ :inside,  :meh, 255 })
    end

    it "is outside when iterations are less than 255" do
      expect(in_or_out({ :meh, 254 })).to eq({ :outside,  :meh, 254 })
      expect(in_or_out({ :meh,  34 })).to eq({ :outside,  :meh,  34 })
    end
  end

end
