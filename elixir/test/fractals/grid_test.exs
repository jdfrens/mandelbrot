defmodule Fractals.GridTest do
  use ExUnit.Case, async: true

  import Complex, warn: false

  alias Fractals.Grid
  alias Fractals.Params

  def params do
    %Params{
      size:        %Fractals.Size{width: 2, height: 3},
      upper_left:  cmplx(-1.0,  1.0),
      lower_right: cmplx( 1.0, -1.0)
    }
  end

  describe ".chunk" do
    test "chunking evenly" do
      params = %Params{chunk_size: 3, chunk_count: 2}
      chunks = Grid.chunk([:a, :b, :c, :d, :e, :f], params) |> Enum.to_list
      assert chunks == [{1, [:a, :b, :c]}, {2, [:d, :e, :f]}]
    end

    test "chunking unevenly" do
      params = %Params{chunk_size: 3, chunk_count: 2}
      chunks = Grid.chunk([:a, :b, :c, :xyz], params) |> Enum.to_list
      assert chunks == [{1, [:a, :b, :c]}, {2, [:xyz]}]
    end
  end

  describe ".grid" do
    test "generate a grid" do
      assert Grid.grid(params) == [
        cmplx(-1.0,  1.0), cmplx(1.0,  1.0),
        cmplx(-1.0,  0.0), cmplx(1.0,  0.0),
        cmplx(-1.0, -1.0), cmplx(1.0, -1.0),
      ]
    end
  end

  describe ".xs" do
    test "generate left-right based on corners and width" do
      assert Grid.xs(params) == [-1.0, 1.0]
    end
  end

  describe ".ys" do
    test "generate top-down based on corners and height" do
      assert Grid.ys(params) == [1.0, 0.0, -1.0]
    end
  end

  describe ".float_sequence" do
    test "generate a sequence" do
      assert Grid.float_sequence(3, -1.0, 1.0) == [-1.0, 0.0, 1.0]
    end
    test "generate the number of requested elements" do
      assert Grid.float_sequence(5, -2.0, 3.0) == [-2.0, -0.75, 0.5, 1.75, 3.0]
    end
    test "sequencing down" do
      assert Grid.float_sequence(3, 1.0, -1.0) == [1.0, 0.0, -1.0]
    end
  end
end
