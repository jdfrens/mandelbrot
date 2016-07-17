defmodule Fractals.Colorizer.RandomSpec do
  use ESpec

  alias Fractals.Params
  alias Fractals.Colorizer.Random

  let :params, do: Params.default

  before do: Random.start_link(params)
  finally do: Process.unregister(Random)

  describe ".at" do
    it "returns a color" do
      expect(Random.at(Random, 127)) |> to(match ~r/\s*\d{1,3}\s+\d{1,3}\s+\d{1,3}/)
    end

    it "returns the same color for the same iterations" do
      color = Random.at(Random, 52)
      expect(Random.at(Random, 52)) |> to(eq color)
      expect(Random.at(Random, 52)) |> to(eq color)
      expect(Random.at(Random, 52)) |> to(eq color)
    end

    it "returns black for max iterations" do
      expect(Random.at(Random, params.max_iterations))
    end
  end
end
