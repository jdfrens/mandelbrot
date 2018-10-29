defmodule Fractals.GridTest do
  use ExUnit.Case, async: true

  alias Fractals.Grid
  alias Fractals.Params

  def params do
    %Params{
      size: %Fractals.Size{width: 2, height: 3},
      upper_left: Complex.new(-1.0, 1.0),
      lower_right: Complex.new(1.0, -1.0)
    }
  end

  describe ".chunk" do
    test "chunking evenly" do
      params = %Params{chunk_size: 3, chunk_count: 2}
      chunks = [:a, :b, :c, :d, :e, :f] |> Grid.chunk(params) |> Enum.to_list()

      assert chunks == [
               %Chunk{number: 1, data: [:a, :b, :c], params: params},
               %Chunk{number: 2, data: [:d, :e, :f], params: params}
             ]
    end

    test "chunking unevenly" do
      params = %Params{chunk_size: 3, chunk_count: 2}
      chunks = [:a, :b, :c, :xyz] |> Grid.chunk(params) |> Enum.to_list()

      assert chunks == [
               %Chunk{number: 1, data: [:a, :b, :c], params: params},
               %Chunk{number: 2, data: [:xyz], params: params}
             ]
    end
  end

  describe ".grid" do
    test "generate a grid" do
      assert Grid.grid(params()) == [
               Complex.new(-1.0, 1.0),
               Complex.new(1.0, 1.0),
               Complex.new(-1.0, 0.0),
               Complex.new(1.0, 0.0),
               Complex.new(-1.0, -1.0),
               Complex.new(1.0, -1.0)
             ]
    end
  end

  describe ".xs" do
    test "generate left-right based on corners and width" do
      assert Grid.xs(params()) == [-1.0, 1.0]
    end
  end

  describe ".ys" do
    test "generate top-down based on corners and height" do
      assert Grid.ys(params()) == [1.0, 0.0, -1.0]
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
