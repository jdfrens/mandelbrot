defmodule Fractals.Colorizer.RandomTest do
  use ExUnit.Case

  alias Fractals.Colorizer.Random
  alias Fractals.Params

  setup do
    Random.start_link(:ok)
    :ok
  end

  describe ".at" do
    test "returning a color" do
      assert Regex.match?(
               ~r/\s*\d{1,3}\s+\d{1,3}\s+\d{1,3}/,
               Random.at(Random, 127, Params.default())
             )
    end

    test "returns the same color for the same iterations" do
      color = Random.at(Random, 52, Params.default())
      assert Random.at(Random, 52, Params.default()) == color
      assert Random.at(Random, 52, Params.default()) == color
      assert Random.at(Random, 52, Params.default()) == color
    end

    test "returning black for max iterations" do
      assert Random.at(Random, Params.default().max_iterations, Params.default()) == PPM.black()
    end
  end

  describe ".pick_colors" do
    test "scales based on max_intensity" do
      colors = [[0.2, 0.5, 0.8]]
      params = %Params{max_intensity: 256}
      assert Random.pick_color(colors, 0, params) == [51, 128, 205]
    end
  end
end
