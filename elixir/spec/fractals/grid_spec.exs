defmodule Fractals.GridSpec do

  use ESpec, async: true

  import Complex, warn: false

  alias Fractals.Grid

  let :options do
    %Fractals.Options{
      size:        %Fractals.Size{ width: 2, height: 3 },
      upper_left:  cmplx(-1.0,  1.0),
      lower_right: cmplx( 1.0, -1.0)
    }
  end

  describe ".generate" do
    it "generates a grid" do
      expect(Grid.generate(options)).to eq([
        { -1.0,  1.0 }, { 1.0,  1.0 },
        { -1.0,  0.0 }, { 1.0,  0.0 },
        { -1.0, -1.0 }, { 1.0, -1.0 },
      ])
    end
  end

  describe ".xs" do
    it "generates left-right based on corners and width" do
      expect(Grid.xs(options)).to eq([-1.0, 1.0])
    end
  end

  describe ".ys" do
    it "generates top-down based on corners and height" do
      expect(Grid.ys(options)).to eq([1.0, 0.0, -1.0])
    end
  end

  describe ".float_sequence" do
    it "generates a sequence" do
      expect(Grid.float_sequence(3, -1.0, 1.0)).to eq([-1.0, 0.0, 1.0])
    end
    it "generates the number of requested elements" do
      expect(Grid.float_sequence(5, -2.0, 3.0)).to eq([-2.0, -0.75, 0.5, 1.75, 3.0])
    end
    it "sequences down" do
      expect(Grid.float_sequence(3, 1.0, -1.0)).to eq([1.0, 0.0, -1.0])
    end
  end

end
