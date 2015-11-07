defmodule Fractals.GeneratorSpec do
  use ESpec

  describe ".header" do
    let :size, do: %Fractals.Size{width: 55, height: 99}
    it "spits out a PPM header including width and height" do
      expect(Fractals.Generator.header(size))
      .to eq(["P3", "55", "99", "255"])
    end
  end

  # more of a property test
  describe ".generate" do
    let :options do
      %Fractals.Options{
        fractal:     :mandelbrot,
        color:       :blue,
        size:        %Fractals.Size{width: 30, height: 20},
        upper_left:  %Complex{real: -2.0, imag:  1.0},
        lower_right: %Complex{real:  1.0, imag: -1.0},
        color_func:  fn _ -> PPM.black end,
        iterator_builder: fn _ -> &(&1) end
      }
    end

    it "generates an image" do
      ppm = Fractals.Generator.generate(options) |> Enum.to_list
      # 4 lines of header + 30 columns * 20 rows
      expect(Enum.count(ppm)).to eq(604)
    end
  end

  describe ".generate (of different concurrencies)" do
    let :options do
      %Fractals.Options{
        fractal:     :mandelbrot,
        color:       :blue,
        size:        %Fractals.Size{width: 30, height: 20},
        upper_left:  %Complex{real: -2.0, imag:  1.0},
        lower_right: %Complex{real:  1.0, imag: -1.0},
        color_func:  fn _ -> PPM.black end,
        iterator_builder: fn _ -> &(&1) end
      }
    end

    # 30 columns * 20 rows
    let :expected_size, do: 600
    let :ppm do
      generate.(options) |> Enum.to_list
    end

    describe "Taskless.generate" do
      let :generate, do: &Fractals.Generator.Taskless.generate/1
      it "generates an image without tasks" do
        expect(Enum.count(ppm)).to eq(expected_size)
      end
    end

    describe "OriginalTasked.generate" do
      let :generate, do: &Fractals.Generator.OriginalTasked.generate/1
      it "generates an image without tasks" do
        expect(Enum.count(ppm)).to eq(expected_size)
      end
    end

    describe "LongerTasked.generate" do
      let :generate, do: &Fractals.Generator.LongerTasked.generate/1
      it "generates an image without tasks" do
        expect(Enum.count(ppm)).to eq(expected_size)
      end
    end
  end

  describe ".fractal_iterate" do
    import Fractals.Generator, only: [ fractal_iterate: 3 ]

    let :next do
      fn z -> z + 1 end
    end

    it "stops due to cutoff" do
      cutoff = fn z -> z >= 0 end
      expect(fractal_iterate(next, cutoff, 1)).to eq({1, 0})
    end

    it "stops later because of cutoff" do
      cutoff = fn z -> z >= 128 end
      expect(fractal_iterate(next, cutoff, 1)).to eq({128, 127})
    end

    it "stops because of the number of iterations" do
      cutoff = fn z -> z >= 500 end
      expect(fractal_iterate(next, cutoff, 1)).to eq({256, 255})
    end
  end

  describe ".in_or_out" do
    import Fractals.Generator, only: [ in_or_out: 1 ]

    it "is inside when iterations are greater than or equal to 255" do
      expect(in_or_out({:meh, 538})).to eq({:inside,  :meh, 538})
      expect(in_or_out({:meh, 256})).to eq({:inside,  :meh, 256})
      expect(in_or_out({:meh, 255})).to eq({:inside,  :meh, 255})
    end

    it "is outside when iterations are less than 255" do
      expect(in_or_out({:meh, 254})).to eq({:outside, :meh, 254})
      expect(in_or_out({:meh,  34})).to eq({:outside, :meh,  34})
    end
  end
end
