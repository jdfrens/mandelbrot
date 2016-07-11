defmodule Fractals.GridSpec do
  use ESpec, async: true

  import Complex, warn: false

  alias Fractals.Grid
  alias Fractals.Params

  let :params do
    %Params{
      size:        %Fractals.Size{width: 2, height: 3},
      upper_left:  cmplx(-1.0,  1.0),
      lower_right: cmplx( 1.0, -1.0)
    }
  end

  describe ".chunk" do
    let :params do
      %Params{
        chunk_size: 3,
        chunk_count: 2
      }
    end

    it "chunks evenly" do
      chunks = Grid.chunk([:a, :b, :c, :d, :e, :f], params)
      expect(chunks |> Enum.to_list)
      |> to(eq([{1, [:a, :b, :c]}, {2, [:d, :e, :f]}]))
    end

    it "chunks unevenly" do
      chunks = Grid.chunk([:a, :b, :c, :xyz], params)
      expect(chunks |> Enum.to_list)
      |> to(eq([{1, [:a, :b, :c]}, {2, [:xyz]}]))
    end
  end

  describe ".grid" do
    it "generates a grid" do
      expect(Grid.grid(params)).to eq([
        cmplx(-1.0,  1.0), cmplx(1.0,  1.0),
        cmplx(-1.0,  0.0), cmplx(1.0,  0.0),
        cmplx(-1.0, -1.0), cmplx(1.0, -1.0),
      ])
    end
  end

  describe ".xs" do
    it "generates left-right based on corners and width" do
      expect(Grid.xs(params)).to eq([-1.0, 1.0])
    end
  end

  describe ".ys" do
    it "generates top-down based on corners and height" do
      expect(Grid.ys(params)).to eq([1.0, 0.0, -1.0])
    end
  end

  describe ".float_sequence" do
    it "generates a sequence" do
      expect(Grid.float_sequence(3, -1.0, 1.0)).to eq([-1.0, 0.0, 1.0])
    end
    it "generates the number of requested elements" do
      expect(Grid.float_sequence(5, -2.0, 3.0))
      .to eq([-2.0, -0.75, 0.5, 1.75, 3.0])
    end
    it "sequences down" do
      expect(Grid.float_sequence(3, 1.0, -1.0)).to eq([1.0, 0.0, -1.0])
    end
  end
end
