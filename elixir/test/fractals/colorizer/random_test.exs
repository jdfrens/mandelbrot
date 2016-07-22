defmodule Fractals.Colorizer.RandomTest do
  use ExUnit.Case

  alias Fractals.Params
  alias Fractals.Colorizer.Random

  setup do
    Random.start_link(Params.default)
    :ok
  end

  describe ".at" do
    test "returning a color" do
      assert Regex.match?(~r/\s*\d{1,3}\s+\d{1,3}\s+\d{1,3}/, Random.at(Random, 127))
    end

    test "returning the same color for the same iterations" do
      color = Random.at(Random, 52)
      assert Random.at(Random, 52) == color
      assert Random.at(Random, 52) == color
      assert Random.at(Random, 52) == color
    end

    test "returning black for max iterations" do
      assert Random.at(Random, Params.default.max_iterations) == PPM.black
    end
  end
end
