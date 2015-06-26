defmodule Mandelbrot.GridSpec do

  use ESpec

  let :options do
    %Mandelbrot.Options{
      size:        %Mandelbrot.Size{ width: 2, height: 3 },
      upper_left:  %Complex{ real: -1.0, imag:  1.0 },
      lower_right: %Complex{ real:  1.0, imag: -1.0 }
    }
  end

  describe ".generate_grid" do
    import Mandelbrot.Grid, only: [ generate_grid: 1 ]

    it "generates a grid" do
      expect(generate_grid(options)).to eq([
        { -1.0,  1.0 }, { 1.0,  1.0 },
        { -1.0,  0.0 }, { 1.0,  0.0 },
        { -1.0, -1.0 }, { 1.0, -1.0 },
      ])
    end
  end

  describe ".xs" do
    it "generates left-right based on corners and width" do
      expect(Mandelbrot.Grid.xs(options)).to eq([-1.0, 1.0])
    end
  end

  describe ".ys" do
    it "generates top-down based on corners and height" do
      expect(Mandelbrot.Grid.ys(options)).to eq([1.0, 0.0, -1.0])
    end
  end

  describe ".float_sequence" do
    import Mandelbrot.Grid, only: [ float_sequence: 3 ]

    it "generates a sequence" do
      expect(float_sequence(3, -1.0, 1.0)).to eq([-1.0, 0.0, 1.0])
    end
    it "generates the number of requested elements" do
      expect(float_sequence(5, -2.0, 3.0)).to eq([-2.0, -0.75, 0.5, 1.75, 3.0])
    end
    it "sequences down" do
      expect(float_sequence(3, 1.0, -1.0)).to eq([1.0, 0.0, -1.0])
    end
  end

end
